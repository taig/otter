package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.operation.NumberOperation
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

trait NumberComponent[+Self[_]](using operation: NumberOperation[Self]):
  def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal] =
    operation.bigDecimal(validation)

  val jBigDecimal: Self[JBigDecimal] = operation.bigDecimal(Validation.valid)

  def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal] =
    operation.bigDecimal(validation)

  val bigDecimal: Self[JBigDecimal] = bigDecimal(Validation.valid)

  def jBigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): Self[JBigInteger] =
    operation.bigInteger(validation)

  val jBigInteger: Self[JBigInteger] = jBigInteger(Validation.valid)

  def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): Self[JBigInteger] =
    operation.bigInteger(validation)

  val bigInteger: Self[JBigInteger] = bigInteger(Validation.valid)

  def float(validation: Validation[Constraint.Primitive.Number, Float]): Self[Float] =
    operation.float(validation)

  val float: Self[Float] = float(Validation.valid)

  def int(validation: Validation[Constraint.Primitive.Number, Int]): Self[Int] = operation.int(validation)

  val int: Self[Int] = int(Validation.valid)

  def long(validation: Validation[Constraint.Primitive.Number, Long]): Self[Long] = operation.long(validation)

  val long: Self[Long] = long(Validation.valid)
