package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.operation.TupleOperation

import scala.annotation.targetName

trait TupleSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: TupleOperation[Self, Value])
    def schemas: Chain[Reference[Value, ?]] = operation.schemas(self)

object TupleSyntax extends TupleSyntax
