package io.taig.otter

import scala.Tuple as STuple
import scala.annotation.implicitNotFound
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

object Convert extends ConvertInstances:
  inline def apply[A, B](using convert: Convert[A, B]): Convert[A, B] = convert

  given identity: [A] => Convert[A, A]:
    override def to(a: A): A = a
    override def from(b: A): A = b

  given product1: [A, B <: Product]
    => (mirror: Mirror.ProductOf[B] { type MirroredElemTypes = A *: EmptyTuple })
    => Convert[A, B]:
    override def to(a: A): B = mirror.fromProduct(a *: EmptyTuple)
    override def from(b: B): A = STuple.fromProductTyped(b).head

  given productN: [A <: STuple, B <: Product]
    => (mirror: Mirror.ProductOf[B] { type MirroredElemTypes = A })
    => Convert[A, B]:
    override def to(a: A): B = mirror.fromProduct(a)
    override def from(b: B): A = STuple.fromProductTyped(b)

  given sum1: [A <: B, B] => (mirror: Mirror.SumOf[B] { type MirroredElemTypes = A *: EmptyTuple }) => Convert[A, B]:
    override def to(a: A): B = a

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    override def from(b: B): A = b.asInstanceOf[A]
