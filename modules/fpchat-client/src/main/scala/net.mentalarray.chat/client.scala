package net.mentalarray.chat

import cats.Applicative
import cats.data.IndexedStateT
import cats.effect._
import cats.instances.all._
import cats.syntax.all._
import com.comcast.ip4s._
import com.monovore.decline._
import fs2.Stream
import fs2.io.tcp.{Socket, SocketGroup}
import net.mentalarray.chat.RichText._

import java.net.ConnectException
import scala.concurrent.duration.FiniteDuration

//noinspection ScalaFileName
object Client extends IOApp {

  type ClientBehavior[F[_], SA, SB, A] = IndexedReaderStateT[Stream[F, *], Console[F], SA, SB, A]
  type ClientApp[F[_], In, St, Out]    = IndexedStateT[Stream[F, *], In, St, Out]
  type ClientSocket[F[_]]              = MessageSocket[F, Protocol.Server.Message, Protocol.Client.Message]

  private val argsParser: Command[SocketAddress[IpAddress]] = Command("fp-client", "FP Chat Client") {
    val addr = Opts
      .option[String]("address", "Address of chat server")
      .mapValidated(ip => IpAddress.fromString(ip).toValidNel("Invalid IP address"))

    val port = Opts
      .option[Int]("port", "Port of chat server")
      .withDefault(25989)
      .mapValidated(p => Port.fromInt(p).toValidNel("Invalid port number"))

    (addr, port).mapN { case (ip, port) => SocketAddress(ip, port) }
  }

  def run(args: List[String]): IO[ExitCode] = argsParser.parse(args) match {
    case Left(help)     => IO(System.err.println(help)).as(ExitCode.Error)
    case Right(address) =>
      Resource.make(RichConsole[IO]("fpchat-client"))(_ => IO.unit).use { implicit console =>
        Blocker[IO].use { blocker =>
          start[IO](blocker)
            .runA(address)
            .compile
            .drain
            .as(ExitCode.Success)
        }
      }
  }

  def start[F[_]: Console: Concurrent: ContextShift: Timer](
    blocker: Blocker,
    reconnectDelay: FiniteDuration = FiniteDuration(5, "seconds")
  ): ClientApp[F, SocketAddress[IpAddress], ClientSocket[F], Unit] = {
    val clientProgram = for {
      _        <- ClientBehavior.connect[F](blocker, reconnectDelay)
      username <- ClientBehavior.chooseUsername[F]
      _        <- ClientBehavior.chat[F](username)
    } yield ()

    clientProgram.withEnv(implicitly[Console[F]])
  }

  object ClientBehavior {

    def apply[F[_], SA, SB, A](
      runK: (Console[F], SA) => Stream[F, (SB, A)]
    ): ClientBehavior[F, SA, SB, A] = IndexedReaderStateT[Stream[F, *], Console[F], SA, SB, A](runK)

    def connect[F[_]: Concurrent: ContextShift: Timer](
      blocker: Blocker,
      reconnectDelay: FiniteDuration
    ): ClientBehavior[F, SocketAddress[IpAddress], ClientSocket[F], Unit] =
      ClientBehavior { (console, address) =>
        val failedToConnect = Stream.eval(console.putTextLn(s"Failed to connect to $address. Retrying in $reconnectDelay"))

        def connectLoop(sock: SocketGroup): Stream[F, Socket[F]] = Stream
          .resource(sock.client[F](address.toInetSocketAddress))
          .handleErrorWith {
            case _: ConnectException => failedToConnect >> connectLoop(sock).delayBy(reconnectDelay)
            case error               => Stream.raiseError[F](error)
          }

        (Stream.eval(console.putTextLn(s"Connecting to server: $address")) >>
          Stream.resource(SocketGroup[F](blocker)).flatMap(connectLoop))
          .flatMap { socket =>
            Stream.eval(MessageSocket.client[F, Protocol.Server.Message, Protocol.Client.Message](socket))
          }
          .map { messageSocket => (messageSocket, ()) }
      }

    private case object UsernameRejected extends Throwable with scala.util.control.NoStackTrace

    def chooseUsername[F[_]](implicit F: Concurrent[F]): ClientBehavior[F, ClientSocket[F], ClientSocket[F], Username] =
      ClientBehavior { (console, socket) =>
        def requestUsername: F[Username] =
          for {
            wantedName <- console.putText(r"What [u]username[/u] do you want? ") *> console.readText
            username   <- if (wantedName.nonEmpty) F.pure(Username(wantedName))
                          else console.putTextLn(r"You [b]must[/b] specify a username!") *> requestUsername
            _          <- console.putTextLn(r"Requesting username [fg:blue]$username[/fg] from server...")
            _          <- socket.write1(Protocol.Client.Command.RequestUsername(username))
          } yield username

        def handleResult: Stream[F, Username] = {
          import Protocol.Server.Command._

          def usernameAccepted(username: Username) =
            console.putTextLn(r"Server [b]accepted[/b] your username. You are chatting as: [u][fg:blue]$username[/!]")

          def usernameRejected(username: Username, reason: String) =
            console.putError(r"Server [b]rejected[/b] your request for username:[fg:blue]${username.value}[/fg] because: [fg:red]$reason[/fg]")

          socket.read
            .evalMap {
              case AcceptedUsername(username)         => usernameAccepted(username) *> Applicative[F].pure(Option(username))
              case RejectedUsername(username, reason) => usernameRejected(username, reason) *> F.raiseError[Option[Username]](UsernameRejected)
              case _ /* any other message */          => Applicative[F].pure(Option.empty[Username])
            }
            .dropWhile(_.isEmpty)
            .collect { case Some(username) => username }
        }

        def readUsername: Stream[F, (ClientSocket[F], Username)] = for {
          username <- Stream.eval(requestUsername)
          _        <- handleResult.handleErrorWith {
                        case UsernameRejected => readUsername
                        case error            => Stream.raiseError[F](error)
                      }
        } yield (socket, username)

        readUsername
      }

    def chat[F[_]](username: Username)(implicit F: Concurrent[F]): ClientBehavior[F, ClientSocket[F], ClientSocket[F], Unit] =
      ClientBehavior { case (_console, socket) =>
        implicit val console = _console

        Stream.eval(console.putTextLn(r"[fg:white]Welcome to [b]fp-chat![/b]") *> console.putTextLn(s"--------------------")) >>
          handleServerMessages[F](socket)
            .concurrently(chatConsole[F](socket, username))
            .map(res => (socket, res))
            .handleErrorWith {
              case Protocol.Server.Event.ServerShuttingDown | Protocol.Client.Event.ClientDisconnected => Stream((socket, ()))
              case _: java.io.IOException                                                              => Stream((socket, ()))
              case _: java.nio.channels.ReadPendingException                                           => Stream((socket, ()))
              case error                                                                               => Stream.raiseError[F](error)
            }
      }

    private def handleServerMessages[F[_]: Console](
      socket: ClientSocket[F]
    )(implicit F: Concurrent[F]): Stream[F, Unit] = {
      import Protocol.Server._

      def showUsersOnline(users: List[Username]) =
        Console[F].putTextLn("Users currently online:") *>
          users.traverse(username => Console[F].putTextLn(r"\t[fg:blue]$username[/fg]")) *>
          Console[F].newline()

      def showUserWhisper(from: Username, message: String) =
        Console[F].putTextLn(r"[bg:grey](Whisper from [fg:blue]$from[/fg]): $message[/bg]")

      def showUserMessage(from: Username, message: String) =
        Console[F].putTextLn(r"[fg:blue]$from[/fg]: $message")

      def showUserEvent(user: Username, what: String) =
        Console[F].putTextLn(
          r"[bg:lightblue]📢 [fg:white]$user[/fg] [fg:dimwhite][u]$what[/u] the channel.[/fg][/bg]"
        )

      def showServerShuttingDown() =
        Console[F].putTextLn(
          r"[bg:lightred][fg:white][b]Server is [bl]shutting down[/bl] now![/b][/fg][/bg]"
        )

      socket.read.evalMap {
        // Commands
        case Command.UsersCurrentlyOnline(users) => showUsersOnline(users)
        case Command.UserWhispered(user, msg)    => showUserWhisper(user, msg)
        case _: Command                          => F.unit
        // Events
        case Event.UserBroadcast(user, msg)      => showUserMessage(user, msg)
        case Event.UserJoined(user)              => showUserEvent(user, "joined")
        case Event.UserLeft(user)                => showUserEvent(user, "left")
        case Event.SystemAlert(_)                => F.unit
        case Event.ServerShuttingDown            => showServerShuttingDown() *> F.raiseError[Unit](Event.ServerShuttingDown)
      }
    }

    private def chatConsole[F[_]: Console](
      socket: ClientSocket[F],
      username: Username
    )(implicit F: Concurrent[F]): Stream[F, Unit] = {

      val showHelp = {
        Console[F].putTextLn("Help:") *>
          Console[F].putTextLn("--------------------") *>
          Console[F].putTextLn(r"Type: '[fg:green][b]/exit[/b][/fg]' to end the chat session.") *>
          Console[F].putTextLn(
            r"Type: '[fg:green][b]/tell[/b][/fg] [fg:blue][u]username[/u][/fg] [u]message[/u]' to send a private message to user: [fg:blue]username[/fg]."
          ) *>
          Console[F].putTextLn(r"Type: '[fg:green][b]/who[/b][/fg]' to see all online users.") *>
          Console[F].putTextLn(r"Type: '[fg:green][b]/help[/b][/fg]' to show these commands again.") *>
          Console[F].putTextLn(r"Type: '[u]message[/u]' to send a message to all online users.") *>
          Console[F].newline()
      }

      def badSystemCommand(text: String) = Console[F].putError(r"Unknown system command: [u]$text[/u]")

      def handleUserConsoleInput(inputString: String): Stream[F, Option[Protocol.Client.Message]] =
        inputString.trim.split(' ').toList match {
          case "/help" :: Nil                        => Stream.eval(showHelp) >> Stream(None)
          case "/who" :: Nil                         => Stream(Some(Protocol.Client.Command.RequestOnlineUsers))
          case "/exit" :: Nil                        => Stream(Some(Protocol.Client.Event.ClientDisconnected))
          case "/tell" :: user :: msg                => Stream(Some(Protocol.Client.Command.WhisperMessage(Username(user), msg.mkString(" "))))
          case sysCmd :: _ if sysCmd.startsWith("/") => Stream.eval(badSystemCommand(sysCmd)) >> Stream(None)
          case message                               =>
            val sendMessage = message.mkString(" ")
            if (sendMessage.trim.isEmpty) Stream(None)
            else Stream(Some(Protocol.Client.Command.BroadcastMessage(sendMessage)))
        }

      import Protocol.Client.Event.ClientDisconnected

      def readConsole: Stream[F, Unit] =
        Stream
          .repeatEval(Console[F].readText(s"[$username] > "))
          .flatMap(handleUserConsoleInput)
          .evalMap {
            case Some(ClientDisconnected) => socket.write1(ClientDisconnected).flatMap(_ => F.raiseError[Unit](ClientDisconnected))
            case Some(msg)                => socket.write1(msg).map(_ => ())
            case None                     => F.unit
          }

      Stream.eval(showHelp) >>
        readConsole
    }

  }

}
