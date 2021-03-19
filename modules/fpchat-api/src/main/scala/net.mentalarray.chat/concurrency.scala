package net.mentalarray.chat

import cats.effect.concurrent.{MVar, MVar2, Ref}
import cats.effect.{BracketThrow, Concurrent}
import cats.syntax.all._

final class MLock[F[_]] private (mvar: MVar2[F, Unit]) {
  def acquire: F[Unit] = mvar.take
  def release: F[Unit] = mvar.put(())
  def waitOne: F[Unit] = mvar.read

  def atomic[A](fa: F[A])(implicit bt: BracketThrow[F]): F[A] = bt.bracket(acquire)(_ => fa)(_ => release)

  def attempt[A](ifUnlocked: F[A], ifLocked: F[A])(implicit bt: BracketThrow[F]): F[A] = bt.flatMap(mvar.tryTake) {
    case Some(_) => bt.guarantee(ifUnlocked)(release)
    case None    => ifLocked
  }

}

object MLock {

  def create[F[_]](startLocked: Boolean = false)(implicit F: Concurrent[F]): F[MLock[F]] = F.map(if (startLocked) MVar[F].empty[Unit] else MVar[F].of[Unit](())) { mvar =>
    new MLock[F](mvar)
  }

}

class ReadableWritable[F[_], A] private (state: Ref[F, A], writeLock: MLock[F])(implicit
  F: Concurrent[F]
) {
  def read: F[A]                      = writeLock.waitOne *> state.get
  def modify[B](f: A => (A, B)): F[B] = writeLock.atomic(state.modify(f))
  def update(f: A => A): F[Unit]      = writeLock.atomic(state.update(f))

  def modifyK[B](modK: A => F[(A, B)]): F[B] = writeLock.atomic(
    for {
      current     <- state.get
      (next, out) <- modK(current)
      _           <- state.set(next)
    } yield out
  )

}

object ReadableWritable {

  def apply[F[_]: Concurrent]: ReadableWritableCreateApplied[F] = CreateAppliedImpl[F]()

  sealed trait ReadableWritableCreateApplied[F[_]] {
    def create[A](initialState: A): F[ReadableWritable[F, A]]
  }

  private final case class CreateAppliedImpl[F[_]]()(implicit F: Concurrent[F]) extends ReadableWritableCreateApplied[F] {

    def create[A](initialState: A): F[ReadableWritable[F, A]] = for {
      ref <- Ref.of[F, A](initialState)
      lck <- MLock.create[F]()
    } yield new ReadableWritable[F, A](ref, lck)

  }

}
