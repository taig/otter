package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.ConstantOperation

import scala.annotation.targetName

trait ConstantSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: ConstantOperation[Self, Value])
    def encode[T](encoder: Encoder[Value, T]): T = operation.encode(self)(encoder)

    def schema: Reference[Value, ?] = operation.schema(self)

object ConstantSyntax extends ConstantSyntax
