package io.taig.openapi.schema

import scala.Product as SProduct
import scala.annotation.implicitNotFound
import scala.deriving.*

object Evidence:
  @implicitNotFound(
    "Can not construct a Product[${A}]: Make sure that all fields of the case class are covered in the correct order"
  )
  trait Product[A]:
    type Out
    def to(a: A): Out
    def from(out: Out): A

  object Product:
    type Aux[A, B] = Evidence.Product[A] { type Out = B }
    inline def apply[A](using evidence: Evidence.Product[A]): evidence.type = evidence
    inline def instance[A, B](f: A => B)(g: B => A): Evidence.Product.Aux[A, B] = new Product[A]:
      override type Out = B
      override def to(a: A): B = f(a)
      override def from(b: B): A = g(b)

    given product1[A <: SProduct, B](using
        mirror: Mirror.ProductOf[A] { type MirroredElemTypes = B *: EmptyTuple }
    ): Evidence.Product.Aux[A, B] =
      instance[A, B](Tuple.fromProductTyped(_).head)(b => mirror.fromProduct(b *: EmptyTuple))

    given productN[A <: SProduct, B <: Tuple](using
        mirror: Mirror.ProductOf[A] { type MirroredElemTypes = B }
    ): Evidence.Product.Aux[A, B] = instance[A, B](Tuple.fromProductTyped)(mirror.fromProduct)

  @implicitNotFound(
    "Can not construct a Sum[${A}]: Make sure that all branches of the enum are covered in the correct order"
  )
  trait Sum[A]:
    type Out
    def to(a: A): Out
    def from(out: Out): A

  object Sum extends SumInstances:
    type Aux[A, B] = Evidence.Sum[A] { type Out = B }
    inline def apply[A](using evidence: Evidence.Sum[A]): evidence.type = evidence
    inline def instance[A, B](f: A => B)(g: B => A): Evidence.Sum.Aux[A, B] = new Sum[A]:
      override type Out = B
      override def to(a: A): B = f(a)
      override def from(b: B): A = g(b)

    given sum1[A, B <: A](using
        mirror: Mirror.SumOf[A] { type MirroredElemTypes = B *: EmptyTuple }
    ): Evidence.Sum.Aux[A, B] = instance[A, B](_.asInstanceOf[B])(identity)
