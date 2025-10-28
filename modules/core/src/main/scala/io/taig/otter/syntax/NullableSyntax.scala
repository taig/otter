package io.taig.otter.syntax

import io.taig.otter.codec.Encoder
import io.taig.otter.operation.NullableOperation
import io.taig.otter.Reference
import scala.annotation.targetName

trait NullableSyntax[Self[_], +Value[_]](using operation: NullableOperation[Self, Value]):
  extension [A](self: Self[A])
    def encode[T](encoder: Encoder[Value, T]): Option[T] = operation.encode(self)(encoder)

    @targetName("nullableSchema")
    def schema: Reference[Value, ?] = operation.schema(self)
