package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.operation.CoerceOperation

import scala.annotation.targetName

trait CoerceSyntax[Self[_], Value[_]](using operation: CoerceOperation[Self, Value]):
  extension [A](self: Self[A])
    @targetName("coerceSchema")
    def schema: Reference[Value, ?] = operation.schema(self)

  extension [A](self: Value[A]) def coerce: Self[A] = operation.coerce(self)
