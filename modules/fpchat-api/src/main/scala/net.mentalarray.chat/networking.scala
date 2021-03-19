package net.mentalarray.chat

import cats.effect.Concurrent
import fs2.concurrent.Queue
import cats.implicits._
import fs2.Stream
import fs2.io.tcp.Socket
import scodec.{Decoder, Encoder}
import scodec.stream.{StreamDecoder, StreamEncoder}
import fs2.concurrent.Topic

/**
  * Socket which reads a stream of messages of type `In` and allows writing
  * messages of type `Out`.
  */
trait MessageSocket[F[_], In, Out] {
  def read: Stream[F, In]
  def write1(out: Out): F[Unit]
}

trait ServerMessageSocket[F[_], In, Out] extends MessageSocket[F, In, Out] {
  def readNoSubscription: Stream[F, In]
}

object MessageSocket {

  def client[F[_]: Concurrent, In: Decoder, Out: Encoder](
    socket: Socket[F],
    outputBound: Int = 256,
    bufferBytes: Int = 1024 * 4
  ): F[MessageSocket[F, In, Out]] = Queue.bounded[F, Out](outputBound).map { outgoing =>
    new MessageSocket[F, In, Out] {
      val inDecoder  = implicitly[Decoder[In]]
      val outEncoder = implicitly[Encoder[Out]]

      val read: Stream[F, In] = {
        val readSocket =
          socket
            .reads(bufferBytes)
            .through(StreamDecoder.many(inDecoder).toPipeByte[F])

        val writeOutput =
          outgoing.dequeue
            .through(StreamEncoder.many(outEncoder).toPipeByte)
            .through(socket.writes())

        readSocket.concurrently(writeOutput)
      }

      def write1(out: Out): F[Unit] = outgoing.offer1(out).map(_ => ())
    }
  }

  def server[F[_]: Concurrent, In: Decoder, Out: Encoder](
    socket: Socket[F],
    events: Topic[F, Out],
    outputBound: Int = 256,
    bufferBytes: Int = 1024 * 4
  ): Stream[F, ServerMessageSocket[F, In, Out]] =
    for {
      serverEvents <- Stream.eval(Concurrent[F].delay(events.subscribe(math.max(8, outputBound / 2))))
      serverQueue  <- Stream.eval(Queue.bounded[F, Out](outputBound))
    } yield new ServerMessageSocket[F, In, Out] {
      val inDecoder  = implicitly[Decoder[In]]
      val outEncoder = implicitly[Encoder[Out]]

      val readNoSubscription: Stream[F, In] = {
        val writeOutput =
          serverQueue.dequeue
            .through(StreamEncoder.many(outEncoder).toPipeByte)
            .through(socket.writes())

        val readSocket =
          socket
            .reads(bufferBytes)
            .through(StreamDecoder.many(inDecoder).toPipeByte[F])

        readSocket.concurrently(writeOutput)
      }

      val read: Stream[F, In] = {
        val writeOutput =
          (serverQueue.dequeue merge serverEvents)
            .through(StreamEncoder.many(outEncoder).toPipeByte)
            .through(socket.writes())

        val readSocket =
          socket
            .reads(bufferBytes)
            .through(StreamDecoder.many(inDecoder).toPipeByte[F])

        readSocket.concurrently(writeOutput)
      }

      def write1(out: Out): F[Unit] = serverQueue.offer1(out).map(_ => ())
    }

}
