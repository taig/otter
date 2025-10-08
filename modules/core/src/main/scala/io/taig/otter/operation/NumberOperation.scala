package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.OperationInvariant

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

  given OperationInvariant[[Shape[_], Self[_[a] <: Shape[a], _]] =>> NumberOperation[[a] =>> Self[Nothing, a]]] with
    extension [Shape[_], Self[_[a] <: Shape[a], _]](operation: NumberOperation[[a] =>> Self[Nothing, a]])
      override def imapK[T[_[a] <: Shape[a], _]](
          fK: [Value[a] <: Shape[a], A] => Self[Value, A] => T[Value, A]
      )(
          gK: [Value[a] <: Shape[a], A] => T[Value, A] => Self[Value, A]
      ): NumberOperation[[a] =>> T[Nothing, a]] = new NumberOperation[[a] =>> T[Nothing, a]]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): T[Nothing, JBigDecimal] = fK(operation.bigDecimal(validation))

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): T[Nothing, JBigInteger] = fK(operation.bigInteger(validation))

        override def double(validation: Validation[Constraint.Primitive.Number, Double]): T[Nothing, Double] =
          fK(operation.double(validation))

        override def float(validation: Validation[Constraint.Primitive.Number, Float]): T[Nothing, Float] =
          fK(operation.float(validation))

        override def int(validation: Validation[Constraint.Primitive.Number, Int]): T[Nothing, Int] =
          fK(operation.int(validation))

        override def long(validation: Validation[Constraint.Primitive.Number, Long]): T[Nothing, Long] =
          fK(operation.long(validation))

        override def constraints[A](self: T[Nothing, A]): Chain[Constraint.Primitive.Number] =
          operation.constraints(gK(self))
