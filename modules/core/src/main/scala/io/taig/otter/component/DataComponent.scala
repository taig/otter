package io.taig.otter.component

import io.taig.data.Data
import io.taig.otter.syntax.AllSyntax.*
import io.taig.otter.operation.UnionOperation
import cats.Invariant

trait DataComponent[Number[a] <: Value[a], Union[_]: Invariant, Value[_]](using UnionOperation[Union, Value])
    extends NumberComponent[Number],
      UnionComponent[Union, Value]:
  val number: Union[Data.Number] =
    branch("jBigDecimal", jBigDecimal) |
      branch("jBigInteger", jBigInteger) |
      branch("long", long) |
      branch("int", int) |
      branch("float", float) |
      branch("double", double)
