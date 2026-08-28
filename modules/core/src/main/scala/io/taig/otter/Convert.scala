package io.taig.otter

import scala.Tuple as STuple
import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.deriving.*

/** Bridges the structural shapes a schema produces (tuples for records, nested `Either` for unions) to nominal types
  * (case classes and enums).
  */
@implicitNotFound(
  "Can not construct a Convert[${A}, ${B}]: Make sure that all fields or branches are covered in the correct order"
)
trait Convert[A, B]:
  def to(a: A): B

  def from(b: B): A

object Convert:
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
