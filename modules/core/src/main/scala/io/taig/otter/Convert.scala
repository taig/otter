package io.taig.otter

import scala.Tuple as STuple
import scala.annotation.implicitNotFound
import scala.deriving.*

@implicitNotFound(
  "Can not construct a Convert[${A}, ${B}]: Make sure that all fields or branches are covered in the correct order"
)
trait Convert[A, B]:
  def to(a: A): B

  def from(b: B): A

object Convert extends ConvertInstances:
  inline def apply[A, B](using convert: Convert[A, B]): Convert[A, B] = convert

  given product1[A, B <: Product](using
      mirror: Mirror.ProductOf[B] { type MirroredElemTypes = A *: EmptyTuple }
  ): Convert[A, B] with
    override def to(a: A): B = mirror.fromProduct(a *: EmptyTuple)
    override def from(b: B): A = STuple.fromProductTyped(b).head

  given productN[A <: STuple, B <: Product](using
      mirror: Mirror.ProductOf[B] { type MirroredElemTypes = A }
  ): Convert[A, B] with
    override def to(a: A): B = mirror.fromProduct(a)
    override def from(b: B): A = STuple.fromProductTyped(b)

  given sum1[A <: B, B](using mirror: Mirror.SumOf[B] { type MirroredElemTypes = A *: EmptyTuple }): Convert[A, B] with
    override def to(a: A): B = a

    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    override def from(b: B): A = b.asInstanceOf[A]