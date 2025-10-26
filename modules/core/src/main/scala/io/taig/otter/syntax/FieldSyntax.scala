package io.taig.otter.syntax

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Merge
import io.taig.otter.Reference
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.RecordOperation

trait FieldSyntax[Self[_], +Record[_]: Invariant, +Value[_]](using
    field: FieldOperation[Self, Value],
    record: RecordOperation[Record, Self]
):
  extension [A](self: Self[A])
    def isOptional: Boolean = field.isOptional(self)

    def name: String = field.name(self)

    def optional: Self[Option[A]] = field.optional(self)

    def optional(default: => A): Self[A] = field.optional(self, default)

    def schema: Reference[Value, ?] = field.schema(self)

    def toRecord: Record[A] = record.lift(self)

    def :*[B](field: Self[B])(using merge: Merge[A, B]): Record[merge.Out] =
      record.zip(toRecord, field.toRecord).imap(merge.apply)(merge.unapply)
