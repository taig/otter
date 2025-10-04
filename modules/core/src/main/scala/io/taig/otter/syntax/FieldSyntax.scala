package io.taig.otter.syntax

import io.taig.otter.operation.RecordOperation

trait FieldSyntax
// extension [Field[_], Record[_], A](self: Field[A])
//   final def toRecord(using operation: RecordOperation[Record, Field]): Record[A] = ???
//   // final def toRecord(using operation: RecordOperation[Record, Field]): Record[A] = operation.lift(self)

object FieldSyntax extends FieldSyntax
