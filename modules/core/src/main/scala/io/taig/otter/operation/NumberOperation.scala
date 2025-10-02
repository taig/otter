package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

trait NumberOperation[Self[_]]:
  self =>

  def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal]

  def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): Self[JBigInteger]

  def double(validation: Validation[Constraint.Primitive.Number, Double]): Self[Double]

  def float(validation: Validation[Constraint.Primitive.Number, Float]): Self[Float]

  def int(validation: Validation[Constraint.Primitive.Number, Int]): Self[Int]

  def long(validation: Validation[Constraint.Primitive.Number, Long]): Self[Long]

  extension [A](self: Self[A]) def constraints: Chain[Constraint]

  def imapK[G[_]](fK: [A] => Self[A] => G[A])(gK: [A] => G[A] => Self[A]): NumberOperation[G] =
    new NumberOperation[G]:
      override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): G[JBigDecimal] =
        fK(self.bigDecimal(validation))

      override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): G[JBigInteger] =
        fK(self.bigInteger(validation))

      override def double(validation: Validation[Constraint.Primitive.Number, Double]): G[Double] =
        fK(self.double(validation))

      override def float(validation: Validation[Constraint.Primitive.Number, Float]): G[Float] =
        fK(self.float(validation))

      override def int(validation: Validation[Constraint.Primitive.Number, Int]): G[Int] = fK(self.int(validation))

      override def long(validation: Validation[Constraint.Primitive.Number, Long]): G[Long] =
        fK(self.long(validation))

      extension [A](ga: G[A]) override def constraints: Chain[Constraint] = self.constraints(gK(ga))

object NumberOperation:
  inline def apply[Self[_]](using operation: NumberOperation[Self]): NumberOperation[Self] = operation

  given InvariantK[NumberOperation] with
    extension [G[_]](self: NumberOperation[G])
      def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): NumberOperation[H] = self.imapK(fK)(gK)
