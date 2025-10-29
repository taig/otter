package io.taig.otter.syntax

import cats.Invariant
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Merge
import io.taig.otter.Reference
import io.taig.otter.operation.RecordOperation

import scala.annotation.targetName

trait RecordSyntax:
  extension [Self[_], Value[_], A](self: Self[A])(using operation: RecordOperation[Self, Value])
    def fields: Chain[Reference[Value, ?]] = operation.fields(self)

  extension [Self[_]: Invariant, Value[_], A](self: Self[A])(using operation: RecordOperation[Self, Value])
    def :*[B](field: Value[B])(using merge: Merge[A, B]): Self[merge.Out] =
      operation.zip(left = self, right = operation.lift(field)).imap(merge.apply)(merge.unapply)

object RecordSyntax extends RecordSyntax
