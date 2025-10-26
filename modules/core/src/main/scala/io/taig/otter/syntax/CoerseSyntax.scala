package io.taig.otter.syntax

import io.taig.otter.operation.CoerceOperation

trait CoerceSyntax[Self[_], -Value[_]](using operation: CoerceOperation[Self, Value]):
  extension [A](self: Value[A]) def coerce: Self[A] = operation.coerce(self)
