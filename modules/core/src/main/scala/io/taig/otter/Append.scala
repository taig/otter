package io.taig.otter

import cats.arrow.Profunctor

import scala.Tuple as STuple

/** Combines product values, keeping the accumulator flat and eliminating visible `Unit` operands. */
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
  ): F[W.Out, R.Out] =
    P.dimap(Z.zip(fa, fb))(W.split)((r: (R1, R2)) => R.join(r._1, r._2))

  /** The result type and the operations that put it together and take it apart.
    *
    * `Out` is selected by the same evidence as `split` and `join`. A separate match type cannot do this for an
    * unbounded opaque member: it cannot prove that the member is disjoint from `Unit` or `NonEmptyTuple`, so it stops
    * reducing before reaching the scalar case. Search can select the scalar instance without inspecting a hidden
    * representation. Only a visible `Unit` is dropped and only a visible tuple accumulator is flattened.
    *
    * Generic callers must pass this evidence through when they want a caller's shape; summoning it against an
    * unconstrained type parameter selects the scalar case at that definition site.
    *
    * Found by implicit search rather than by matching on the schema itself, for two reasons. Search is total, so a
    * direction a schema does not have still yields an instance instead of leaving the append undecided -- and an
    * instance for a direction nothing can reach is never asked to do anything. And nothing is inlined per member, where
    * matching on the schema copied it into every branch, which cost a wide record exponentially more to compile than a
    * narrow one: five members compiled in a second, ten in forty, thirteen not at all.
    *
    * Writing the four instances as one `inline given` over `summonFrom` was measured and rejected. It is the only
    * inline formulation that is even correct -- an `inline match` on `erasedValue` errors where the scrutinee is
    * neither a subtype of a pattern nor provably disjoint from one, and `Any` and `Nothing` both arrive here from a
    * member that goes only one way -- and it stays linear in the width of a record, so it does not bring the blowup
    * above back. It is simply slower: a record of forty members cost 29% more to compile and one of eighty 34% more.
    * And it has to cast at the dispatch as well as inside each instance, because `summonFrom` gives every case one
    * result type where search unifies each instance at its own. The ladder is what Scala 3 offers for ordering
    * instances, and here it is also the cheaper of the two.
    */
  sealed abstract class Shape[A, B]:
    type Out

    def split(value: Out): (A, B)

    def join(a: A, b: B): Out

  object Shape extends Append.LeftUnit:
    type Aux[A, B, O] = Append.Shape[A, B] { type Out = O }

    /** Dropping the right `Unit` takes precedence over flattening a tuple accumulator. */
    given right: [A] => Append.Shape.Aux[A, Unit, A] = new Append.Shape[A, Unit]:
      override type Out = A

      override def split(value: A): (A, Unit) = (value, ())

      override def join(a: A, b: Unit): A = a

  private[otter] trait LeftUnit extends Append.TupleLeft:
    given left: [B] => Append.Shape.Aux[Unit, B, B] = new Append.Shape[Unit, B]:
      override type Out = B

      override def split(value: B): (Unit, B) = ((), value)

      override def join(a: Unit, b: B): B = b

  private[otter] trait TupleLeft extends Append.Pair:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    given tuple: [A <: NonEmptyTuple, B] => Append.Shape.Aux[A, B, STuple.Append[A, B]] = new Append.Shape[A, B]:
      override type Out = STuple.Append[A, B]

      override def split(value: STuple.Append[A, B]): (A, B) =
        val tuple = value.asInstanceOf[NonEmptyTuple]
        (tuple.init.asInstanceOf[A], tuple.last.asInstanceOf[B])

      override def join(a: A, b: B): STuple.Append[A, B] = a :* b

  private[otter] trait Pair:
    given pair: [A, B] => Append.Shape.Aux[A, B, (A, B)] = new Append.Shape[A, B]:
      override type Out = (A, B)

      override def split(value: (A, B)): (A, B) = value

      override def join(a: A, b: B): (A, B) = (a, b)
