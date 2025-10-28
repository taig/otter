package io.taig.otter.syntax

import io.taig.otter.codec.Encoder
import io.taig.otter.operation.ConstantOperation
import io.taig.otter.Reference

trait ConstantSyntax[Self[_], Value[_]](using operation: ConstantOperation[Self, Value]):
  extension [A](self: Self[A])
    def encode[T](encoder: Encoder[Value, T]): T = operation.encode(self)(encoder)

    def schema: Reference[Value, ?] = operation.schema(self)
