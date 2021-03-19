package net.mentalarray

import cats.data.IndexedStateT
import cats.effect.Concurrent
import fs2.Stream
import fs2.concurrent.Topic
import fs2.io.tcp.Socket

package object chat {

  type ServerBehavior[F[_], A]  = IndexedReaderStateT[Stream[F, *], Console[F], ChatServer[F], ChatServer[F], A]
  type ServerApp[F[_], In, Out] = IndexedStateT[Stream[F, *], In, ChatServer[F], Out]
  type ServerSocket[F[_]]       = ServerMessageSocket[F, Protocol.Client.Message, Protocol.Server.Message]

  object ServerBehavior {

    def apply[F[_], A](runK: (Console[F], ChatServer[F]) => Stream[F, A]): ServerBehavior[F, A] =
      IndexedReaderStateT[Stream[F, *], Console[F], ChatServer[F], ChatServer[F], A] { (console, server) =>
        runK(console, server).map(server -> _)
      }

  }

  object ServerSocket {

    def apply[F[_]: Concurrent](events: Topic[F, Protocol.Server.Message])(socket: Socket[F]): Stream[F, ServerSocket[F]] =
      MessageSocket.server[F, Protocol.Client.Message, Protocol.Server.Message](socket, events)

  }

}
