package net.mentalarray.chat

import cats._
import cats.data.IndexedStateT

class IndexedReaderStateT[F[_], E, SA, SB, A](val runF: F[(E, SA) => F[(SB, A)]]) extends Serializable {

  def flatMap[SC, B](bind: A => IndexedReaderStateT[F, E, SB, SC, B])(implicit F: FlatMap[F]): IndexedReaderStateT[F, E, SA, SC, B] = IndexedReaderStateT.shift {
    F.map(runF) { runK => (e: E, sa: SA) =>
      F.flatMap(runK(e, sa)) { case (sb, a) => bind(a).run(e, sb) }
    }
  }

  def flatMapF[B](fmap: A => F[B])(implicit F: FlatMap[F]): IndexedReaderStateT[F, E, SA, SB, B] = IndexedReaderStateT.shift {
    F.map(runF) { runK => (e: E, sa: SA) =>
      F.flatMap(runK(e, sa)) { case (sb, a) => F.map(fmap(a))(b => (sb, b)) }
    }
  }

  def mapK[G[_]](nat: F ~> G)(implicit F: Functor[F]): IndexedReaderStateT[G, E, SA, SB, A] = IndexedReaderStateT.applyF {
    nat(
      F.map(runF) { runK => (e: E, sa: SA) =>
        nat(runK(e, sa))
      }
    )
  }

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedReaderStateT[F, E, SA, SB, B] = transform { case (s, a) => (s, f(a)) }

  def local[EE](f: EE => E)(implicit F: Functor[F]): IndexedReaderStateT[F, EE, SA, SB, A] = IndexedReaderStateT.applyF {
    F.map(runF) { runK => (ee: EE, sa: SA) =>
      runK(f(ee), sa)
    }
  }

  def run(env: E, initial: SA)(implicit F: FlatMap[F]): F[(SB, A)] = F.flatMap(runF)(_.apply(env, initial))

  def runS(env: E, initial: SA)(implicit F: FlatMap[F]): F[SB] = F.map(run(env, initial)) { case (sb, _) => sb }

  def withEnv(env: E)(implicit F: Monad[F]): IndexedStateT[F, SA, SB, A] =
    IndexedStateT { (initial: SA) =>
      F.flatMap(runF)(f => f(env, initial))
    }

  def transform[SC, B](f: (SB, A) => (SC, B))(implicit F: Functor[F]): IndexedReaderStateT[F, E, SA, SC, B] =
    IndexedReaderStateT.applyF {
      F.map(runF) { runK => (e: E, sa: SA) =>
        F.map(runK(e, sa))(f.tupled)
      }
    }

}

private[chat] trait CommonReaderStateTConstructors {

  def pure[F[_], E, S, A](a: A)(implicit F: Applicative[F]): IndexedReaderStateT[F, E, S, S, A] = IndexedReaderStateT((_, s) => F.pure(s -> a))

  def liftF[F[_], E, S, A](fa: F[A])(implicit F: Applicative[F]): IndexedReaderStateT[F, E, S, S, A] = IndexedReaderStateT((_, s) => F.map(fa)(s -> _))

  def inspect[F[_], E, S, A](f: S => A)(implicit F: Applicative[F]): IndexedReaderStateT[F, E, S, S, A] = IndexedReaderStateT((_, s) => F.pure(s -> f(s)))

  def inspectF[F[_], E, S, A](f: S => F[A])(implicit F: Applicative[F]): IndexedReaderStateT[F, E, S, S, A] = IndexedReaderStateT((_, s) => F.map(f(s))(s -> _))

  def get[F[_], E, S](implicit F: Applicative[F]): IndexedReaderStateT[F, E, S, S, S] = IndexedReaderStateT((_, s) => F.pure(s -> s))
}

object IndexedReaderStateT extends AnyRef with CommonReaderStateTConstructors {

  def apply[F[_], E, SA, SB, A](runK: (E, SA) => F[(SB, A)])(implicit F: Applicative[F]): IndexedReaderStateT[F, E, SA, SB, A] = new IndexedReaderStateT(F.pure(runK))

  def applyF[F[_], E, SA, SB, A](runF: F[(E, SA) => F[(SB, A)]]): IndexedReaderStateT[F, E, SA, SB, A] = new IndexedReaderStateT(runF)

  def modify[F[_], E, SA, SB](f: SA => SB)(implicit F: Applicative[F]): IndexedReaderStateT[F, E, SA, SB, Unit] = IndexedReaderStateT((_, s) => F.pure((f(s), ())))

  def modifyF[F[_], E, SA, SB](modK: SA => F[SB])(implicit F: Applicative[F]): IndexedReaderStateT[F, E, SA, SB, Unit] =
    IndexedReaderStateT((_, s) => F.map(modK(s))(sb => (sb, ())))

  /** Internal API - shifts the execution of `run` in the `F` context. */
  private def shift[F[_], E, SA, SB, A](runF: F[(E, SA) => F[(SB, A)]])(implicit F: FlatMap[F]): IndexedReaderStateT[F, E, SA, SB, A] = F match {
    case ap: Applicative[F] @unchecked => IndexedReaderStateT.apply[F, E, SA, SB, A]((e: E, sa: SA) => F.flatMap(runF)(f => f(e, sa)))(ap)
    case _                             => IndexedReaderStateT.applyF(runF)
  }

}
