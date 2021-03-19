package net.mentalarray.chat

import scodec.Codec
import scodec.codecs._
import scodec.codecs.implicits._

import scala.util.control.NoStackTrace

/** All messages sent from server or client */
object Protocol {

  /** Messages sent from a chat server */
  object Server {

    /** Base trait for all of the messages sent from a server */
    sealed trait Message

    object Message {

      implicit val codec: Codec[Message] = discriminated[Message]
        .by(byte)
        .typecase(0, Codec[Command])
        .typecase(1, Codec[Event])

    }

    /** Base trait for all commands sent from the server to a specific client */
    sealed trait Command extends Message

    object Command {
      case class AcceptedUsername(username: Username)                 extends Command
      case class RejectedUsername(username: Username, reason: String) extends Command
      case class UsersCurrentlyOnline(users: List[Username])          extends Command
      case class UserWhispered(from: Username, message: String)       extends Command

      implicit val codec: Codec[Command] = discriminated[Command]
        .by(byte)
        .typecase(0, Codec[AcceptedUsername])
        .typecase(1, Codec[RejectedUsername])
        .typecase(2, Codec[UsersCurrentlyOnline])
        .typecase(3, Codec[UserWhispered])

    }

    /** Base trait for all events sent from the server to all clients */
    sealed trait Event extends Message

    object Event {
      case class UserJoined(username: Username)                 extends Event
      case class UserLeft(username: Username)                   extends Event
      case class SystemAlert(message: String)                   extends Event
      case class UserBroadcast(from: Username, message: String) extends Event
      case object ServerShuttingDown                            extends Throwable with Event with NoStackTrace

      implicit val codec: Codec[Event] = discriminated[Event]
        .by(byte)
        .typecase(0, Codec[UserJoined])
        .typecase(1, Codec[UserLeft])
        .typecase(2, Codec[SystemAlert])
        .typecase(3, Codec[UserBroadcast])
        .typecase(4, Codec[ServerShuttingDown.type])

    }

  }

  object Client {

    /** Base trait for all of the messages sent from a client */
    sealed trait Message

    object Message {

      implicit val codec: Codec[Message] = discriminated[Message]
        .by(byte)
        .typecase(0, Codec[Command])
        .typecase(1, Codec[Event])

    }

    /** Base trait for all commands sent from a client */
    sealed trait Command extends Message

    object Command {
      case class RequestUsername(username: Username)                 extends Command
      case class BroadcastMessage(message: String)                   extends Command
      case class WhisperMessage(username: Username, message: String) extends Command
      case object RequestOnlineUsers                                 extends Command

      implicit val codec: Codec[Command] = discriminated[Command]
        .by(byte)
        .typecase(0, Codec[RequestUsername])
        .typecase(1, Codec[BroadcastMessage])
        .typecase(2, Codec[WhisperMessage])
        .typecase(3, Codec[RequestOnlineUsers.type])

    }

    /** Base trait for all events sent from a client */
    sealed trait Event extends Message

    object Event {
      case object ClientDisconnected extends Throwable with Event with NoStackTrace

      implicit val codec: Codec[Event] = discriminated[Event]
        .by(byte)
        .typecase(0, Codec[ClientDisconnected.type])

    }

  }

}
