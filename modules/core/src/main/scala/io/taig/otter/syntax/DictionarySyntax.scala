package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Constraint

import scala.annotation.targetName

trait DictionarySyntax[Self[_], Value[_]](using operation: DictionaryOperation[Self, Value]):
  extension [A](self: Self[A])
    @targetName("dictionaryConstraints")
    def constraints: Chain[Constraint.Object] = operation.constraints(self)

    @targetName("dictionarySchema")
    def schema: Reference[Value, ?] = operation.schema(self)
