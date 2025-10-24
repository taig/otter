package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.operation.NumberOperation
import io.taig.validation.Validation

trait NumberComponent[+Self[_]](using operation: NumberOperation[Self]):
  def int(validation: Validation[Constraint.Primitive.Number, Int]): Self[Int] = operation.int(validation)

  val int: Self[Int] = int(Validation.valid)

  def long(validation: Validation[Constraint.Primitive.Number, Long]): Self[Long] = operation.long(validation)

  val long: Self[Long] = long(Validation.valid)

// def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal] =
//   operation.bigDecimal(validation)

// val jBigDecimal: Self[JBigDecimal] = operation.bigDecimal(Validation.valid)

// def jBigDecimal(
//     minimum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
//     maximum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
//     multiple: Undefined.Or[JBigDecimal] = Undefined
// ): Self[JBigDecimal] = operation.bigDecimal(validation.std.jBigDecimal(minimum, maximum, multiple))
