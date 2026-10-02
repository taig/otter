package io.taig.otter

import cats.arrow.Profunctor

/** The mirror of [[Append]]: `*:` carries its accumulator on the right, so that is the tuple it keeps flat. */
object Prepend:
  /** Prepends `fa` to `fb`, flattening each direction on its own terms, as [[Append.apply]] does and for the same
    * reason: a child that only writes reads `Any`, and a child that only reads writes `Nothing`.
    */
  def apply[F[-_, +_], W1, R1, W2, R2](fa: F[W1, R1], fb: F[W2, R2])(using
      P: Profunctor[F],
      Z: Zip[F],
      W: Prepend.Shape[W1, W2],
      R: Prepend.Shape[R1, R2]
  ): F[W.Out, R.Out] =
    P.dimap(Z.zip(fa, fb))(W.split)((r: (R1, R2)) => R.join(r._1, r._2))

  /** The result type and the operations that put it together and take it apart. Found by implicit search rather than by
    * matching on the schema, for the reasons [[Append.Shape]] is -- including the measured one against writing the four
    * instances as a single `inline given`.
    */
  sealed abstract class Shape[A, B]:
    type Out

    def split(value: Out): (A, B)

    def join(a: A, b: B): Out

  object Shape extends Prepend.RightUnit:
    type Aux[A, B, O] = Prepend.Shape[A, B] { type Out = O }

    /** Dropping the left `Unit` takes precedence over flattening a tuple accumulator. */
    given left: [B] => Prepend.Shape.Aux[Unit, B, B] = new Prepend.Shape[Unit, B]:
      override type Out = B

      override def split(value: B): (Unit, B) = ((), value)

      override def join(a: Unit, b: B): B = b

  private[otter] trait RightUnit extends Prepend.TupleRight:
    given right: [A] => Prepend.Shape.Aux[A, Unit, A] = new Prepend.Shape[A, Unit]:
      override type Out = A

      override def split(value: A): (A, Unit) = (value, ())

      override def join(a: A, b: Unit): A = a

  private[otter] trait TupleRight extends Prepend.Pair:
    given tuple: [A, B <: NonEmptyTuple] => Prepend.Shape.Aux[A, B, A *: B] = new Prepend.Shape[A, B]:
      override type Out = A *: B

      override def split(value: A *: B): (A, B) = (value.head, value.tail)

      override def join(a: A, b: B): A *: B = a *: b

  private[otter] trait Pair:
    given pair: [A, B] => Prepend.Shape.Aux[A, B, (A, B)] = new Prepend.Shape[A, B]:
      override type Out = (A, B)

      override def split(value: (A, B)): (A, B) = value

      override def join(a: A, b: B): (A, B) = (a, b)
