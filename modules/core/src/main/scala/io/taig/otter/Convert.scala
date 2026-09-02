package io.taig.otter

import scala.Tuple as STuple
import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.deriving.*
import scala.util.NotGiven

/** Bridges the structural shapes a schema produces (tuples for records, nested `Either` for unions) to nominal types
  * (case classes and enums).
  *
  * Both directions at once, which is what a schema that round trips asks for. A schema that only reads asks for
  * [[Convert.Reader]], and gets further with it: putting a value together is a weaker demand than taking one apart.
  */
@implicitNotFound(
  "Can not construct a Convert[${A}, ${B}]: Make sure that all fields or branches are covered in the correct order"
)
trait Convert[A, B] extends Convert.Reader[A, B]:
  def from(b: B): A

object Convert:
  /** The read half, which is all a schema that only reads can ask for.
    *
    * Taking a `B` apart has to know which member of the shape it belongs to, so [[Convert.sum]] pins the branches to
    * the target's members by type and reaches them by position. Putting one together does not: a union hands back the
    * branch that matched, and that branch has already read whatever it reads, so the nesting is all there is to
    * discard.
    *
    * That difference is what lets a union of branches that have stopped being distinguishable still be read. A branch
    * reading a case that holds no members is typed by that case's singleton, and a schema's read side is covariant, so
    * the type variable it is inferred into is instantiated from below and widened to the enum. Nothing is lost with the
    * singleton -- the branch still reads that very case -- and nothing here needs it back.
    */
  @implicitNotFound("Can not construct a Convert.Reader[${A}, ${B}]: Make sure that every branch reads a ${B}")
  trait Reader[A, B]:
    def to(a: A): B

  object Reader extends Convert.ReaderBranch:
    inline def apply[A, B](using reader: Convert.Reader[A, B]): Convert.Reader[A, B] = reader

    /** The nesting `:+` builds, read from both sides, so a union collapses at any depth.
      *
      * Only where [[Convert]] has nothing to offer, so a union whose branches can still be told apart is still put back
      * together by position -- and still has the order of its branches checked while it is.
      */
    given union: [L, R, B] => (
        absent: NotGiven[Convert[Either[L, R], B]],
        left: Convert.Reader[L, B],
        right: Convert.Reader[R, B]
    ) => Convert.Reader[Either[L, R], B]:
      override def to(a: Either[L, R]): B = a.fold(left.to, right.to)

  /** A branch, which is where the nesting ends. Comes after [[Convert.Reader.union]] so that a union read as an
    * `Either` -- or as `Any` -- collapses rather than standing for one of its branches.
    */
  private[otter] trait ReaderBranch:
    given branch: [A, B] => (absent: NotGiven[Convert[A, B]], evidence: A <:< B) => Convert.Reader[A, B]:
      override def to(a: A): B = evidence(a)

  /** The left nested `Either` that `:+` builds for a sum whose members are `T`.
    *
    * `(A, B, C)` becomes `Either[Either[A, B], C]`, matching the association of `a :+ b :+ c`.
    */
  type Coproduct[T <: STuple] = T match
    case h *: EmptyTuple => h
    case h *: t          => Convert.Nested[h, t]

  type Nested[A, T <: STuple] = T match
    case h *: EmptyTuple => Either[A, h]
    case h *: t          => Convert.Nested[Either[A, h], t]

  inline def apply[A, B](using convert: Convert[A, B]): Convert[A, B] = convert

  given identity: [A] => Convert[A, A]:
    override def to(a: A): A = a
    override def from(b: A): A = b

  /** A product with no fields: an empty case class, or an enum case declared without parameters.
    *
    * There is nothing to carry, so `Unit` is the whole of its structural shape. That is what a record of constants
    * writes -- a branch tagged by its `type` and nothing else -- and without this instance such a record has to be
    * mapped onto its case by hand.
    */
  given product0: [B <: Product]
    => (mirror: Mirror.ProductOf[B] { type MirroredElemTypes = EmptyTuple }) => Convert[Unit, B]:
    override def to(a: Unit): B = mirror.fromProduct(EmptyTuple)
    override def from(b: B): Unit = ()

  given product1: [A, B <: Product]
    => (mirror: Mirror.ProductOf[B] { type MirroredElemTypes = A *: EmptyTuple }) => Convert[A, B]:
    override def to(a: A): B = mirror.fromProduct(a *: EmptyTuple)
    override def from(b: B): A = STuple.fromProductTyped(b).head

  given productN: [A <: STuple, B <: Product]
    => (mirror: Mirror.ProductOf[B] { type MirroredElemTypes = A }) => Convert[A, B]:
    override def to(a: A): B = mirror.fromProduct(a)
    override def from(b: B): A = STuple.fromProductTyped(b)

  given sum1: [A <: B, B] => (mirror: Mirror.SumOf[B] { type MirroredElemTypes = A *: EmptyTuple }) => Convert[A, B]:
    override def to(a: A): B = a

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    override def from(b: B): A = b.asInstanceOf[A]

  /** Every sum of two or more members, at any arity.
    *
    * The nesting is walked at run time and its shape is pinned by [[Coproduct]], so this replaces what used to be one
    * generated instance per arity and no longer stops at 22.
    */
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  given sum: [X, Y, B] => (
      mirror: Mirror.SumOf[B],
      arity: ValueOf[STuple.Size[mirror.MirroredElemTypes]],
      evidence: Either[X, Y] =:= Convert.Coproduct[mirror.MirroredElemTypes]
  ) => Convert[Either[X, Y], B]:
    override def to(a: Either[X, Y]): B = Convert.project(a, arity.value).asInstanceOf[B]

    override def from(b: B): Either[X, Y] =
      Convert.inject(mirror.ordinal(b), arity.value, b).asInstanceOf[Either[X, Y]]

  /** Unwraps `arity - 1` levels of nesting to reach the member that is present. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  @tailrec
  private def project(value: Any, arity: Int): Any =
    if arity <= 1 then value
    else
      value.asInstanceOf[Either[Any, Any]] match
        case Right(value) => value
        case Left(value)  => project(value, arity - 1)

  /** Wraps a member in the nesting that its ordinal occupies: `Right` unless it is the last, then `Left` all the way
    * out.
    */
  private def inject(ordinal: Int, arity: Int, value: Any): Any =
    nest(if ordinal == 0 then value else Right(value), levels = arity - 1 - ordinal)

  @tailrec
  private def nest(value: Any, levels: Int): Any =
    if levels <= 0 then value else nest(Left(value), levels - 1)
