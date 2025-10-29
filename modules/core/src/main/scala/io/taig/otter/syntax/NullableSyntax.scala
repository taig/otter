package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.NullableOperation

import scala.annotation.targetName

trait NullableSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: NullableOperation[Self, Value])
    def encode[T](encoder: Encoder[Value, T]): Option[T] = operation.encode(self)(encoder)

    def schema: Reference[Value, ?] = operation.schema(self)

  extension [Self[_], Value[_], A](self: Value[A])(using operation: NullableOperation[Self, Value])
    def nullable: Self[Option[A]] = operation.nullable(self)

    def nullable(default: => A): Self[A] = operation.nullable(self, default)

object NullableSyntax extends NullableSyntax
