package io.taig.otter.syntax

import io.taig.otter.codec.Encoder
import io.taig.otter.operation.NullableOperation

trait NullableSyntax[Self[_], +Value[_]](using operation: NullableOperation[Self, Value]):
  extension [A](self: Self[A]) def encode[T](encoder: Encoder[Value, T]): Option[T] = operation.encode(self)(encoder)
