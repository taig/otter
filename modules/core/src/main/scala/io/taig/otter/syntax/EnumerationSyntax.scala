package io.taig.otter.syntax

import cats.data.NonEmptyList
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.EnumerationOperation

trait EnumerationSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: EnumerationOperation[Self, Value])
    def encode[T](encoder: Encoder[Value, T]): NonEmptyList[T] =
      operation.encode(self)(encoder)

object EnumerationSyntax extends EnumerationSyntax
