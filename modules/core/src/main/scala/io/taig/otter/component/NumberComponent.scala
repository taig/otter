package io.taig.otter.component

import io.taig.Undefined
import io.taig.validation.Validation
import io.taig.validation
import io.taig.validation.Constraint
import java.math.BigDecimal as JBigDecimal
import io.taig.validation.Comparison
import io.taig.otter.operation.NumberOperation

trait NumberComponent[+Self[_]](using operation: NumberOperation[Self])
//   def jBigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal] =
//     schema.bigDecimal(validation)

//   val jBigDecimal: Self[JBigDecimal] = schema.bigDecimal(Validation.valid)

//   def jBigDecimal(
//       minimum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
//       maximum: Undefined.Or[Comparison[JBigDecimal]] = Undefined,
//       multiple: Undefined.Or[JBigDecimal] = Undefined
//   ): Self[JBigDecimal] = schema.bigDecimal(validation.std.jBigDecimal(minimum, maximum, multiple))
