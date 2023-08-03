package io.taig.otter.schema

import scala.Product as SProduct
import scala.annotation.implicitNotFound
import scala.deriving.*

object Evidence:
  @implicitNotFound(
    "Can not construct an Evidence.Product[${A}]: Make sure that all fields of the case class are covered in the correct order"
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
    "Can not construct an Evidence.Coproduct[${A}]: Make sure that all branches of the enum are covered in the correct order"
  )
  trait Coproduct[A]:
    type Out
    def to(a: A): Out
    def from(out: Out): A

  object Coproduct extends CoproductInstances:
    type Aux[A, B] = Evidence.Coproduct[A] { type Out = B }
    inline def apply[A](using evidence: Evidence.Coproduct[A]): evidence.type = evidence
    inline def instance[A, B](f: A => B)(g: B => A): Evidence.Coproduct.Aux[A, B] = new Coproduct[A]:
      override type Out = B
      override def to(a: A): B = f(a)
      override def from(b: B): A = g(b)

    given coproduct1[A, B <: A](using
        mirror: Mirror.SumOf[A] { type MirroredElemTypes = B *: EmptyTuple }
    ): Evidence.Coproduct.Aux[A, B] = instance[A, B](_.asInstanceOf[B])(identity)
