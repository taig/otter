package io.taig.otter

import cats.arrow.Profunctor

/** The value shape that results from prepending `A` to `B`, keeping tuples flat and eliminating `Unit`.
  *
  * The mirror of [[Append]], and it flattens on the other side for the reason the two operators differ: `:*` carries
  * what it has built on the left, `*:` on the right, so the operand that is already a tuple is the one each keeps flat.
  */
type Prepend[A, B] = B match
  case _ *: _ =>
    A match
      case Unit => B
      case _    => A *: B
  case Unit => A
  case _    =>
    A match
      case Unit => B
      case _    => A *: B *: EmptyTuple

object Prepend:
  /** Prepends `fa` to `fb`, flattening each direction on its own terms, as [[Append.apply]] does and for the same
    * reason: a child that only writes reads `Any`, and a child that only reads writes `Nothing`.
    */
  def apply[F[-_, +_], W1, R1, W2, R2](fa: F[W1, R1], fb: F[W2, R2])(using
      P: Profunctor[F],
      Z: Zip[F],
      W: Prepend.Shape[W1, W2],
      R: Prepend.Shape[R1, R2]
  ): F[Prepend[W1, W2], Prepend[R1, R2]] =
    P.dimap(Z.zip(fa, fb))(W.split)((r: (R1, R2)) => R.join(r._1, r._2))

  /** How a value of `Prepend[A, B]` is put together and taken apart. Found by implicit search rather than by matching
    * on the schema, for the reasons [[Append.Shape]] is.
    */
  sealed abstract class Shape[A, B]:
    def split(value: Prepend[A, B]): (A, B)

    def join(a: A, b: B): Prepend[A, B]

  object Shape extends Prepend.RightUnit:
    /** Prepending nothing leaves the accumulator as it is, whatever the accumulator is, so this has to come before
      * [[Prepend.TupleRight]], which would otherwise take a tuple nothing was prepended to apart into a head and a
      * tail. [[Append.Shape.right]] outranks [[Append.TupleLeft]] for the mirror of this reason.
      */
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given left: [B] => Prepend.Shape[Unit, B]:
      override def split(value: Prepend[Unit, B]): (Unit, B) = ((), value.asInstanceOf[B])

      override def join(a: Unit, b: B): Prepend[Unit, B] = b.asInstanceOf[Prepend[Unit, B]]

  private[otter] trait RightUnit extends Prepend.TupleRight:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given right: [A] => Prepend.Shape[A, Unit]:
      override def split(value: Prepend[A, Unit]): (A, Unit) = (value.asInstanceOf[A], ())

      override def join(a: A, b: Unit): Prepend[A, Unit] = a.asInstanceOf[Prepend[A, Unit]]

  private[otter] trait TupleRight extends Prepend.Pair:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given tuple: [A, B <: NonEmptyTuple] => Prepend.Shape[A, B]:
      override def split(value: Prepend[A, B]): (A, B) =
        val tuple = value.asInstanceOf[NonEmptyTuple]
        (tuple.head.asInstanceOf[A], tuple.tail.asInstanceOf[B])

      override def join(a: A, b: B): Prepend[A, B] = (a *: b).asInstanceOf[Prepend[A, B]]

  private[otter] trait Pair:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given pair: [A, B] => Prepend.Shape[A, B]:
      override def split(value: Prepend[A, B]): (A, B) = value.asInstanceOf[(A, B)]

      override def join(a: A, b: B): Prepend[A, B] = (a, b).asInstanceOf[Prepend[A, B]]
