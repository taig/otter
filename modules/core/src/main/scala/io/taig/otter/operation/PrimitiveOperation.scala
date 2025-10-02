package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.validation.Validation

import java.math.BigInteger

trait PrimitiveOperation[Self[_]] extends BooleanOperation[Self], NumberOperation[Self], StringOperation[Self]:
  self =>

  extension [A](self: Self[A]) override def constraints: Chain[Constraint.Primitive.Number | Constraint.Primitive.Text]

  override def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): PrimitiveOperation[G] =
    new PrimitiveOperation[G]:
      override def boolean: G[Boolean] = fK(self.boolean)

      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, java.math.BigDecimal]
      ): G[java.math.BigDecimal] =
        fK(self.bigDecimal(validation))

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
        override def constraints: Chain[Constraint.Primitive.Number | Constraint.Primitive.Text] =
          self.constraints(gK(ga))

object PrimitiveOperation:
  inline def apply[Self[_]](using operation: PrimitiveOperation[Self]): PrimitiveOperation[Self] = operation

  given invariant: InvariantK[PrimitiveOperation] with
    extension [G[_]](self: PrimitiveOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): PrimitiveOperation[H] =
        self.imapK(fK)(gK)
