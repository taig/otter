package io.taig.otter.syntax

import cats.data.NonEmptyList
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.EnumerationOperation

trait EnumerationSyntax[Self[_], Value[_]](using operation: EnumerationOperation[Self, Value]):
  extension [A](self: Self[A])
    def encode[T](encoder: Encoder[Value, T]): NonEmptyList[T] =
      operation.encode(self)(encoder)
