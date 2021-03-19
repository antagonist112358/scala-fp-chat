package net.mentalarray.chat

import cats._
import cats.implicits._
import cats.effect._
import com.comcast.ip4s._
import com.monovore.decline._
import fs2.Stream
import fs2.io.tcp._

//noinspection ScalaFileName
object Server extends IOApp {

  private val argsParser: Command[SocketAddress[IpAddress]] = Command("fp-client", "FP Chat Client") {
    val addr = Opts
      .option[String]("address", "Bind address of the chat server")
      .withDefault("0.0.0.0")
      .mapValidated(ip => IpAddress.fromString(ip).toValidNel("Invalid IP address"))

    val port = Opts
      .option[Int]("port", "Bind port of chat server")
      .withDefault(25989)
      .mapValidated(p => Port.fromInt(p).toValidNel("Invalid port number"))

    (addr, port).mapN { case (ip, port) => SocketAddress(ip, port) }
  }

  override def run(args: List[String]): IO[ExitCode] = argsParser.parse(args) match {
    case Left(help)     => IO(System.err.println(help)).as(ExitCode.Error)
    case Right(address) =>
      Resource.make(Console.stdio[IO])(_ => IO.unit).use { implicit console =>
        Blocker[IO].use { blocker =>
          start[IO](address, blocker).compile.drain.as(ExitCode.Success)
        }
      }
  }

  def start[F[_]: Console: Concurrent: ContextShift](
    address: SocketAddress[IpAddress],
    blocker: Blocker
  ) = {
    val consoleF = implicitly[Console[F]]
    val console  = consoleF.mapK(concurrentArrowStream[F])

    def createClientHandler(chatServer: ChatServer[F]) =
      (clientSocket: Socket[F]) =>
        (for {
          connClient <- ServerHandlers.negotiateUsername[F](clientSocket)
          _          <- ServerHandlers.chatClientHandler[F](connClient)
        } yield ())
          .withEnv(implicitly[Console[F]])
          .runA(chatServer)

    def gracefulShutdown(server: ChatServer[F]) =
      consoleF.putTextLn("Shutting down server...") *>
        server.broadcastEvent(Protocol.Server.Event.ServerShuttingDown)

    def serverHandler(
      serverState: ChatServer[F],
      socketGroup: SocketGroup,
      clientHandler: Socket[F] => Stream[F, Unit]
    ) = {
      socketGroup
        .server[F](address.toInetSocketAddress)
        .map { socketResource =>
          Stream
            .resource(socketResource)
            .flatMap(clientHandler)
            .handleErrorWith {
              case Protocol.Client.Event.ClientDisconnected => Stream.empty
              case _: java.io.IOException                   => Stream.empty
              case otherError                               => Stream.raiseError[F](otherError)
            }
            .scope
        }
        .parJoinUnbounded
      //.onFinalize(gracefulShutdown(serverState))
    }

    for {
      _            <- console.putTextLn(s"Starting fp-chat server on: $address")
      serverState  <- Stream.eval(ChatServer[F])
      sockGroup    <- Stream.resource(SocketGroup[F](blocker))
      clientHandler = createClientHandler(serverState)
      _            <- console.putTextLn("Server started. Waiting for client connections...")
      _            <- serverHandler(serverState, sockGroup, clientHandler)
    } yield ()
  }

  private def concurrentArrowStream[F[_]]: F ~> Stream[F, *] =
    new (F ~> Stream[F, *]) {
      final def apply[A](fa: F[A]): Stream[F, A] = Stream.eval[F, A](fa)
    }

}

object ServerHandlers {

  private case class UsernameNotAvailable(username: Username) extends Throwable with scala.util.control.NoStackTrace

  def negotiateUsername[F[_]: Concurrent](rawSocket: Socket[F]): ServerBehavior[F, ConnectedClient[F]] =
    ServerBehavior { (console, server) =>
      def handleNegotiation(serverSocket: ServerSocket[F]) = {
        import Protocol.Client.Command._
        import Protocol.Server.{Command => ServerCommand}

        def usernameAccepted(username: Username, connectedClient: ConnectedClient[F]) =
          serverSocket.write1(ServerCommand.AcceptedUsername(username)) *>
            Applicative[F].pure(Option(connectedClient))

        def handleClientCommands: Stream[F, ConnectedClient[F]] =
          serverSocket.readNoSubscription
            .evalMap {
              case RequestUsername(username) =>
                server.tryReserveUsername(username, serverSocket).flatMap {
                  case Some(connectedClient) => usernameAccepted(username, connectedClient)
                  case None                  => Concurrent[F].raiseError[Option[ConnectedClient[F]]](UsernameNotAvailable(username))
                }
              case _ /* any other message */ => Applicative[F].pure(Option.empty[ConnectedClient[F]])
            }
            .dropWhile(_.isEmpty)
            .collect { case Some(connClient) => connClient }
            .handleErrorWith {
              case UsernameNotAvailable(username) => Stream.eval(serverSocket.write1(ServerCommand.RejectedUsername(username, "Not available."))) >> handleClientCommands
              case otherError                     => Stream.raiseError[F](otherError)
            }

        handleClientCommands
      }

      for {
        serverSocket    <- ServerSocket(server.eventsTopic)(rawSocket)
        remoteAddress   <- Stream.eval(rawSocket.remoteAddress)
        resolvedSocket  <- Stream.bracket(Concurrent[F].pure(serverSocket))(_ => console.putTextLn(s"Client `$remoteAddress` disconnected."))
        connectedClient <- handleNegotiation(resolvedSocket)
        _               <- Stream.eval(console.putTextLn(s"Client `$remoteAddress` is registered as username: ${connectedClient.username}"))
      } yield connectedClient
    }

  def chatClientHandler[F[_]: Concurrent: ContextShift](client: ConnectedClient[F]): ServerBehavior[F, Unit] =
    ServerBehavior { (console, server) =>
      import Protocol.Client.Command._
      import Protocol.Client.Event

      def clientDisconnected(disconnected: ConnectedClient[F]) =
        server.unregister(disconnected.id) *>
          server.broadcastEvent(Protocol.Server.Event.UserLeft(disconnected.username)) *>
          console.putTextLn(s"User `${disconnected.username}` disconnected.") *>
          Concurrent[F].raiseError[Unit](Event.ClientDisconnected)

      Stream
        .constant(client)
        .evalTap(connClient => server.broadcastEvent(Protocol.Server.Event.UserJoined(connClient.username)))
        .flatMap { connClient =>
          val userid   = connClient.id
          val username = connClient.username
          val socket   = connClient.socket

          socket.read.evalMap {
            case RequestUsername(noReassign) => socket.write1(Protocol.Server.Command.RejectedUsername(noReassign, s"Username already assigned to $username"))
            case BroadcastMessage(message)   => server.broadcastMessage(userid, message)
            case WhisperMessage(to, message) => server.whisperMessage(userid, to, message)
            case RequestOnlineUsers          => server.onlineUsers.map(Protocol.Server.Command.UsersCurrentlyOnline).flatMap(socket.write1)
            case Event.ClientDisconnected    => Concurrent[F].raiseError[Unit](Event.ClientDisconnected)
          }
        }
        .handleErrorWith {
          case Event.ClientDisconnected => Stream.eval(clientDisconnected(client))
          case _: java.io.IOException   => Stream.eval(clientDisconnected(client))
          case otherError               => Stream.raiseError[F](otherError)
        }
    }

}
