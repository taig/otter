package io.taig.crock.schema

import cats.data.Chain
import io.taig.crock.validation.*

import scala.annotation.targetName

abstract class Schema[A]:
  type Self[a] <: Schema[a] { type Self[a] = Schema.this.Self[a] }

  def constraints: Chain[Constraint]

  abstract class Property[B]:
    def value: B
    def modify(f: B => B): Self[A]
    final def apply(b: B): Self[A] = modify(_ => b)

  object Property:
    abstract class Optional[B] extends Property[Option[B]]:
      @targetName("as")
      final def apply(b: B): Self[A] = apply(Some(b))
      final def clear: Self[A] = apply(None)

    object Optional:
      def apply[B](b: Option[B], g: (Option[B] => Option[B]) => Self[A]): Property.Optional[B] = new Optional[B]:
        override def value: Option[B] = b
        override def modify(f: Option[B] => Option[B]): Self[A] = g(f)

      def apply[B, C](
          schema: Schema[B],
          property: schema.type => schema.Property.Optional[B],
          copy: Option[B] => Self[A],
          validation: Validation[B, C],
          g: C => B
      ): Property.Optional[C] = new Optional[C]:
        override def value: Option[C] = property(schema).value.flatMap(validation(_).toOption)
        override def modify(f: Option[C] => Option[C]): Self[A] =
          copy(f(property(schema).value.flatMap(validation(_).toOption)).map(g))

  def description: Property.Optional[String]
  def example: Property.Optional[A]

  def optional: Self[Option[A]]
  def isOptional: Boolean

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

  final infix def zip[B](other: Schema[B]): Product[(A, B)] =
    Product.Zip(toProduct, other.toProduct, Product.Properties.Empty)

  final def toProduct: Product[A] = this match
    case schema: Product[?] => schema
    case schema             => Product(schema)

object Schema extends ToSchemaOps:
  abstract class Value[A] extends Schema[A]:
    override type Self[a] <: Value[a] { type Self[a] = Value.this.Self[a] }

final class SchemaOps[A](self: Schema[A]) extends AnyVal:
  inline def :*[B](other: Schema[B]): Product[(A, B)] = self.zip(other)
  inline def *:[B](other: Schema[B]): Product[(B, A)] = other.zip(self)
  inline def :*(other: Schema[Unit]): Product[A] = self.zip(other).imap { case (a, _) => a }((_, ()))
  inline def *:(other: Schema[Unit]): Product[A] = other.zip(self).imap { case (_, a) => a }(((), _))
final class SchemaOpsUnit(self: Schema[Unit]) extends AnyVal:
  inline def :*[A](other: Schema[A]): Product[A] = other :* self
  inline def *:[A](other: Schema[A]): Product[A] = other :* self
final class SchemaOpsTuple[A <: Tuple](self: Schema[A]) extends AnyVal:
  inline def :*[B](other: Schema[B]): Product[Tuple.Append[A, B]] =
    self.zip(other).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
  inline def *:[B](other: Schema[B]): Product[B *: A] =
    other.zip(self).imap { case (b, a) => b *: a } { case b *: a => (b, a) }
  inline def :*(other: Schema[Unit]): Product[A] = self.zip(other).imap { case (a, _) => a }((_, ()))
  inline def *:(other: Schema[Unit]): Product[A] = other.zip(self).imap { case (_, a) => a }(((), _))

trait ToSchemaOps extends ToSchemaOps1:
  implicit final def toSchemaOpsTuple[A <: Tuple](self: Schema[A]): SchemaOpsTuple[A] = new SchemaOpsTuple[A](self)
  implicit final def toSchemaOpsUnit(self: Schema[Unit]): SchemaOpsUnit = new SchemaOpsUnit(self)
trait ToSchemaOps1:
  implicit final def toSchemaOps[A](self: Schema[A]): SchemaOps[A] = new SchemaOps[A](self)
