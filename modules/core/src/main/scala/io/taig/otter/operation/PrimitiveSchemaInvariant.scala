package io.taig.otter.operation

import io.taig.validation.Constraint
import io.taig.validation.Validation

import cats.data.Chain
import java.math.BigInteger
import io.taig.otter.Annotation
import io.taig.otter.Primitive

trait PrimitiveSchemaInvariant[Self[_]]
    extends BooleanSchemaInvariant[Self],
      NumberSchemaInvariant[Self, Constraint.Primitive],
      StringSchemaInvariant[Self, Constraint.Primitive]:
  self =>

  extension [A](self: Self[A]) override def constraints: Chain[Constraint.Primitive]

  final override def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): PrimitiveSchemaInvariant[G] =
    new PrimitiveSchemaInvariant[G]:
      override def boolean: G[Boolean] = fK(self.boolean)

      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, java.math.BigDecimal]
      ): G[java.math.BigDecimal] = fK(self.bigDecimal(validation))

      override def bigInteger(validation: Validation[Constraint.Primitive.Number, BigInteger]): G[BigInteger] =
        fK(self.bigInteger(validation))

      override def double(validation: Validation[Constraint.Primitive.Number, Double]): G[Double] =
        fK(self.double(validation))

      override def float(validation: Validation[Constraint.Primitive.Number, Float]): G[Float] =
        fK(self.float(validation))

      override def int(validation: Validation[Constraint.Primitive.Number, Int]): G[Int] =
        fK(self.int(validation))

      override def long(validation: Validation[Constraint.Primitive.Number, Long]): G[Long] =
        fK(self.long(validation))

      override def string(validation: Validation[Constraint.Primitive.Text, String]): G[String] =
        fK(self.string(validation))

      override def parser[A](name: String, decode: String => Either[String, A], encode: A => String): G[A] =
        fK(self.parser(name, decode, encode))

      extension [A](ga: G[A])
        override def constraints: Chain[Constraint.Primitive] = self.constraints(gK(ga))

        override def imap[B](f: A => B)(g: B => A): G[B] = fK(self.imap(gK(ga))(f)(g))

object PrimitiveSchemaInvariant:
  inline def apply[Self[_]](using invariant: PrimitiveSchemaInvariant[Self]): PrimitiveSchemaInvariant[Self] =
    invariant

  given PrimitiveSchemaInvariant[[a] =>> Annotation[Primitive[a]]] with
    export BooleanSchemaInvariant.schema.boolean
    export NumberSchemaInvariant.schema.{bigDecimal, bigInteger, double, float, int, long}
    export StringSchemaInvariant.schema.{parser, string}

    extension [A](self: Annotation[Primitive[A]])
      override def constraints: Chain[Constraint.Primitive] = self.self.constraints

      override def imap[B](f: A => B)(g: B => A): Annotation[Primitive[B]] = self.map(_.imap(f)(g))
