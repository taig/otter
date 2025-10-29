package io.taig.otter.component

import io.taig.data.Data
import io.taig.otter.operation.NumberOperation
import io.taig.validation.Validation
import io.taig.otter.operation.UnionOperation

trait DataComponent[Number[a] <: Value[a], Union[_], Value[_]](using UnionOperation[Union, Value])
    extends NumberComponent[Number]:
  val number: Union[Data.Number] = ???
  // UnionOperation[Union, Value].lift(jBigDecimal)
  // jBigDecimal | jBigInteger
  // jBigDecimal | jBigInteger | long | int | float | double
