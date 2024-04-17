package io.taig.otter

import scala.Product as SProduct
import scala.Tuple as STuple
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
    def instance[A, B](f: A => B)(g: B => A): Evidence.Product.Aux[A, B] = new Product[A]:
      override type Out = B
      override def to(a: A): B = f(a)
      override def from(b: B): A = g(b)

    given product1[A <: SProduct, B](using
        mirror: Mirror.ProductOf[A] { type MirroredElemTypes = B *: EmptyTuple }
    ): Evidence.Product.Aux[A, B] =
      instance[A, B](STuple.fromProductTyped(_).head)(b => mirror.fromProduct(b *: EmptyTuple))

    given productN[A <: SProduct, B <: STuple](using
        mirror: Mirror.ProductOf[A] { type MirroredElemTypes = B }
    ): Evidence.Product.Aux[A, B] = instance[A, B](STuple.fromProductTyped)(mirror.fromProduct)

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
    def instance[A, B](f: A => B)(g: B => A): Evidence.Coproduct.Aux[A, B] = new Coproduct[A]:
      override type Out = B
      override def to(a: A): B = f(a)
      override def from(b: B): A = g(b)

    given coproduct1[A, B <: A](using
        mirror: Mirror.SumOf[A] { type MirroredElemTypes = B *: EmptyTuple }
    ): Evidence.Coproduct.Aux[A, B] = instance[A, B](_.asInstanceOf[B])(identity)

  trait Merge[A, B]:
    type Out
    def apply(ab: (A, B)): Out
    def unapply(out: Out): (A, B)

  object Merge extends Merge1:
    type Aux[A, B, C] = Evidence.Merge[A, B] { type Out = C }
    inline def apply[A, B](using evidence: Evidence.Merge[A, B]): evidence.type = evidence

    given [A]: Merge.Aux[A, Unit, A] = instance[A, Unit, A] { case (a, _) => a }(a => (a, ()))

    given [A]: Merge.Aux[Unit, A, A] = instance[Unit, A, A] { case (_, a) => a }(a => ((), a))

    given [A <: STuple, B]: Merge.Aux[A, B, STuple.Append[A, B]] =
      instance[A, B, STuple.Append[A, B]] { case (a, b) => a :* b } { ab =>
        (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B])
      }

  trait Merge1:
    def instance[A, B, C](f: ((A, B)) => C)(g: C => (A, B)): Evidence.Merge.Aux[A, B, C] = new Merge[A, B]:
      override type Out = C
      override def apply(ab: (A, B)): C = f(ab)
      override def unapply(out: C): (A, B) = g(out)

    given [A, B]: Merge.Aux[A, B, (A, B)] = instance[A, B, (A, B)](identity)(identity)
