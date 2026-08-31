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
    * One `inline match` classifies both directions together rather than one per direction, which is right as long as
    * they agree on shape. A one directional child is where they part: its write slot is `Nothing`, so the pair falls
    * through to the general branch and `Append` cannot reduce the write side at all, Scala declining to reduce a match
    * type over an uninhabited selector. That is harmless in itself -- `Nothing` conforms to the stuck type, so a reader
    * ascription absorbs it -- but it means the write side has to be projected out by index and cast. `head` and `tail`
    * are match types too, and they refuse to reduce for the same reason. The read side builds its tuple rather than
    * taking one apart, so it needs no such care.
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
            P.dimap(Z.zip(fa, fb))((w: W1 *: W2 *: EmptyTuple) =>
              (w.productElement(0).asInstanceOf[W1], w.productElement(1).asInstanceOf[W2])
            )((r: (R1, R2)) => r._1 *: r._2 *: EmptyTuple)
              .asInstanceOf[F[Append[W1, W2], Append[R1, R2]]]
