package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.InvariantK
import scala.deriving.Mirror

trait NumberOperation[Self[_]]:
  def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal]

  def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): Self[JBigInteger]

  def double(validation: Validation[Constraint.Primitive.Number, Double]): Self[Double]

  def float(validation: Validation[Constraint.Primitive.Number, Float]): Self[Float]

  def int(validation: Validation[Constraint.Primitive.Number, Int]): Self[Int]

  def long(validation: Validation[Constraint.Primitive.Number, Long]): Self[Long]

  def constraints[A](self: Self[A]): Chain[Constraint.Primitive.Number]

object NumberOperation:
  inline def apply[Self[_]](using operation: NumberOperation[Self]): NumberOperation[Self] = operation

  given InvariantK[NumberOperation] with
    extension [G[_]](operation: NumberOperation[G])
      override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): NumberOperation[H] =
        new NumberOperation[H]:
          override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): H[JBigDecimal] =
            fK(operation.bigDecimal(validation))

          override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): H[JBigInteger] =
            fK(operation.bigInteger(validation))

          override def double(validation: Validation[Constraint.Primitive.Number, Double]): H[Double] =
            fK(operation.double(validation))

          override def float(validation: Validation[Constraint.Primitive.Number, Float]): H[Float] =
            fK(operation.float(validation))

          override def int(validation: Validation[Constraint.Primitive.Number, Int]): H[Int] =
            fK(operation.int(validation))

          override def long(validation: Validation[Constraint.Primitive.Number, Long]): H[Long] =
            fK(operation.long(validation))

          override def constraints[A](self: H[A]): Chain[Constraint.Primitive.Number] = operation.constraints(gK(self))
