package io.taig.otter

import cats.arrow.Profunctor

import scala.Tuple as STuple

/** The value shape that results from appending `B` to `A`, keeping tuples flat and eliminating `Unit`. */
type Append[A, B] = A match
  case _ *: _ =>
    B match
      case Unit => A
      case _    => STuple.Append[A, B]
  case Unit => B
  case _    =>
    B match
      case Unit => A
      case _    => A *: B *: EmptyTuple

object Append:
  /** Appends `fb` to `fa`, flattening both directions at once.
    *
    * The write and read shapes of a schema built with this operation stay in lockstep, which is why one `inline match`
    * suffices instead of one per direction.
    */
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  inline def apply[F[-_, +_] <: Matchable, W1, R1, W2, R2](fa: F[W1, R1], fb: F[W2, R2])(using
      P: Profunctor[F],
      Z: Zip[F]
  ): F[Append[W1, W2], Append[R1, R2]] =
    inline fa match
      case fa: F[wx *: wy, rx *: ry] =>
        inline fb match
          case fb: F[Unit, Unit] =>
            P.dimap(Z.zip(fa, fb))((w: wx *: wy) => (w, ()))((r: (rx *: ry, Unit)) => r._1)
              .asInstanceOf[F[Append[W1, W2], Append[R1, R2]]]
          case fb =>
            P.dimap(Z.zip(fa, fb))((w: STuple.Append[wx *: wy, W2]) =>
              (w.init.asInstanceOf[wx *: wy], w.last.asInstanceOf[W2])
            )((r: (rx *: ry, R2)) => r._1 :* r._2)
              .asInstanceOf[F[Append[W1, W2], Append[R1, R2]]]
      case fa: F[Unit, Unit] =>
        P.dimap(Z.zip(fa, fb))((w: W2) => ((), w))((r: (Unit, R2)) => r._2)
          .asInstanceOf[F[Append[W1, W2], Append[R1, R2]]]
      case fa =>
        inline fb match
          case fb: F[Unit, Unit] =>
            P.dimap(Z.zip(fa, fb))((w: W1) => (w, ()))((r: (R1, Unit)) => r._1)
              .asInstanceOf[F[Append[W1, W2], Append[R1, R2]]]
          case fb =>
            P.dimap(Z.zip(fa, fb))((w: W1 *: W2 *: EmptyTuple) => (w.head, w.tail.head))((r: (R1, R2)) =>
              r._1 *: r._2 *: EmptyTuple
            ).asInstanceOf[F[Append[W1, W2], Append[R1, R2]]]
