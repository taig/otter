package io.taig.otter.syntax

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Merge
import io.taig.otter.Reference
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.RecordOperation

import scala.annotation.targetName

trait FieldSyntax:

  extension [Self[_], Value[_], A](self: Self[A])(using field: FieldOperation[Self, Value])
    def isOptional: Boolean = field.isOptional(self)

    def name: String = field.name(self)

    def optional: Self[Option[A]] = field.optional(self)

    def optional(default: => A): Self[A] = field.optional(self, default)

    def schema: Reference[Value, ?] = field.schema(self)

  extension [Self[_], Value[_], A](self: Value[A])(using operation: RecordOperation[Self, Value])
    def toRecord: Self[A] = operation.lift(self)

  extension [Field[_], Record[_]: Invariant, A](
      self: Field[A]
  )(using record: RecordOperation[Record, Field])(using FieldOperation[Field, Record])
    def :*[B](field: Field[B])(using merge: Merge[A, B]): Record[merge.Out] =
      record.zip(self.toRecord, field.toRecord).imap(merge.apply)(merge.unapply)

object FieldSyntax extends FieldSyntax
