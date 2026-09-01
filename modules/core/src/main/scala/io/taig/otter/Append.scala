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
  /** Appends `fb` to `fa`, flattening each direction on its own terms.
    *
    * The two directions are classified separately, because they do not always agree on shape: a child that only writes
    * reads `Any`, and a child that only reads writes `Nothing`. Classifying them together let the direction a schema
    * does not have decide the shape of the one it does, so appending anything after a write only member took the record
    * apart against the wrong shape and put every later member in the wrong slot.
    */
  def apply[F[-_, +_], W1, R1, W2, R2](fa: F[W1, R1], fb: F[W2, R2])(using
      P: Profunctor[F],
      Z: Zip[F],
      W: Append.Shape[W1, W2],
      R: Append.Shape[R1, R2]
  ): F[Append[W1, W2], Append[R1, R2]] =
    P.dimap(Z.zip(fa, fb))(W.split)((r: (R1, R2)) => R.join(r._1, r._2))

  /** How a value of `Append[A, B]` is put together and taken apart.
    *
    * Found by implicit search rather than by matching on the schema itself, for two reasons. Search is total, so a
    * direction a schema does not have still yields an instance instead of leaving the append undecided -- and an
    * instance for a direction nothing can reach is never asked to do anything. And nothing is inlined per member, where
    * matching on the schema copied it into every branch, which cost a wide record exponentially more to compile than a
    * narrow one: five members compiled in a second, ten in forty, thirteen not at all.
    */
  sealed abstract class Shape[A, B]:
    def split(value: Append[A, B]): (A, B)

    def join(a: A, b: B): Append[A, B]

  object Shape extends Append.LeftUnit:
    /** Appending nothing leaves the receiver as it is, whatever the receiver is, so this comes before every other. */
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given right: [A] => Append.Shape[A, Unit]:
      override def split(value: Append[A, Unit]): (A, Unit) = (value.asInstanceOf[A], ())

      override def join(a: A, b: Unit): Append[A, Unit] = a.asInstanceOf[Append[A, Unit]]

  private[otter] trait LeftUnit extends Append.TupleLeft:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given left: [B] => Append.Shape[Unit, B]:
      override def split(value: Append[Unit, B]): (Unit, B) = ((), value.asInstanceOf[B])

      override def join(a: Unit, b: B): Append[Unit, B] = b.asInstanceOf[Append[Unit, B]]

  private[otter] trait TupleLeft extends Append.Pair:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given tuple: [A <: NonEmptyTuple, B] => Append.Shape[A, B]:
      override def split(value: Append[A, B]): (A, B) =
        val tuple = value.asInstanceOf[NonEmptyTuple]
        (tuple.init.asInstanceOf[A], tuple.last.asInstanceOf[B])

      override def join(a: A, b: B): Append[A, B] = (a :* b).asInstanceOf[Append[A, B]]

  private[otter] trait Pair:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given pair: [A, B] => Append.Shape[A, B]:
      override def split(value: Append[A, B]): (A, B) = value.asInstanceOf[(A, B)]

      override def join(a: A, b: B): Append[A, B] = (a, b).asInstanceOf[Append[A, B]]
