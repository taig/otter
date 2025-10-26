package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.operation.RecordOperation
import io.taig.otter.Merge
import cats.Invariant
import cats.syntax.all.*
import scala.annotation.targetName

trait RecordSyntax[Self[_]: Invariant, Value[_]](using record: RecordOperation[Self, Value]):
  extension [A](self: Self[A])
    def fields: Chain[Reference[Value, ?]] = record.fields(self)

    @targetName("appendField")
    def :*[B](field: Value[B])(using merge: Merge[A, B]): Self[merge.Out] =
      record.zip(left = self, right = record.lift(field)).imap(merge.apply)(merge.unapply)
