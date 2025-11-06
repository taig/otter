package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.operation.CoerceOperation

import scala.annotation.targetName

trait CoerceSyntax
//   extension [Self[_], Value[_], A](self: Self[A])(using operation: CoerceOperation[Self, Value])
//     def schema: Reference[Value, ?] = operation.schema(self)

//   extension [Self[_], Value[_], A](self: Value[A])(using operation: CoerceOperation[Self, Value])
//     def coerce: Self[A] = operation.coerce(self)

// object CoerceSyntax extends CoerceSyntax
