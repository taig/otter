package io.taig.otter.syntax

import io.taig.otter.operation.RecordOperation

trait FieldSyntax:
  extension [Field[_], Record[_], A](self: Field[A])(using operation: RecordOperation[Record, Field])
    final def toRecord: Record[A] = operation.lift(self)

object FieldSyntax extends FieldSyntax
