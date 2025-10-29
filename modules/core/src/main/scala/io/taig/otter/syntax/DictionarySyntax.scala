package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Constraint

import scala.annotation.targetName

trait DictionarySyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: DictionaryOperation[Self, Value])
    def constraints: Chain[Constraint.Object] = operation.constraints(self)

    def schema: Reference[Value, ?] = operation.schema(self)

object DictionarySyntax extends DictionarySyntax
