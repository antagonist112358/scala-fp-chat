package net.mentalarray.chat

import java.util.UUID

import cats.data.OptionT
import cats.effect._
import cats.syntax.all._
import fs2.concurrent.Topic

import scala.collection.immutable._

/** An instance of a connected `client`, including identifier and username. */
case class ConnectedClient[F[_]](
  id: UUID,
  username: Username,
  socket: ServerSocket[F]
)

final class ChatServer[F[_]] private (val eventsTopic: Topic[F, Protocol.Server.Message], connectedUsers: ReadableWritable[F, Map[UUID, ConnectedClient[F]]])(implicit
  F: Concurrent[F]
) {

  def tryReserveUsername(username: Username, socket: ServerSocket[F]) = connectedUsers.modify { clients =>
    // Username is not available
    if (clients.values.map(_.username).toSet.contains(username)) (clients, Option.empty[ConnectedClient[F]])
    // Username is available
    else {
      val clientId   = UUID.randomUUID()
      val client     = ConnectedClient(clientId, username, socket)
      val updatedMap = clients + (clientId -> client)

      (updatedMap, Some(client))
    }
  }

  def get(id: UUID): F[Option[ConnectedClient[F]]] = connectedUsers.read.map(_.get(id))
  def unregister(id: UUID)                         = connectedUsers.update(cc => cc - id)

  def byName(username: Username): F[Option[ConnectedClient[F]]] = connectedUsers.read.map(_.values.find(cc => cc.username === username))

  def broadcastMessage(fromId: UUID, message: String) = get(fromId).flatMap {
    case Some(sourceClient) => eventsTopic.publish1(Protocol.Server.Event.UserBroadcast(sourceClient.username, message))
    case None               => connectedUsers.read.map(_.keySet.contains(fromId)).ifM(unregister(fromId), F.unit)
  }

  def whisperMessage(fromId: UUID, to: Username, message: String) =
    OptionT(get(fromId))
      .flatMap { sourceClient =>
        OptionT(byName(to)).semiflatTap { targetClient =>
          targetClient.socket.write1(
            Protocol.Server.Command.UserWhispered(sourceClient.username, message)
          )
        }
      }
      .value
      .map(_ => ())

  def broadcastEvent(event: Protocol.Server.Event) = eventsTopic.publish1(event)

  def onlineUsers: F[List[Username]] = connectedUsers.read.map(_.values.map(_.username).toList)
}

object ChatServer {

  def apply[F[_]: Concurrent]: F[ChatServer[F]] =
    for {
      users <- ReadableWritable[F].create(Map.empty[UUID, ConnectedClient[F]])
      topic <- Topic[F, Protocol.Server.Message](Protocol.Server.Event.SystemAlert("Server starting..."))
    } yield new ChatServer[F](topic, users)

}
