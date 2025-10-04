package io.taig.otter.component

import io.taig.Undefined
import io.taig.otter.operation.NumberOperation
import io.taig.validation
import io.taig.validation.Validation
import io.taig.otter.Constraint
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.validation.Comparison

trait NumberComponent[+Self[_]](using operation: NumberOperation[Self]):
  def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal] =
    operation.bigDecimal(validation)

  val jBigDecimal: Self[JBigDecimal] = operation.bigDecimal(Validation.valid)

  def jBigDecimal(
      minimum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
      maximum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
      multiple: Undefined.Or[JBigDecimal] = Undefined
  ): Self[JBigDecimal] = operation.bigDecimal(validation.std.jBigDecimal(minimum, maximum, multiple))
