package io.taig.otter.component

import io.taig.otter.syntax.InvariantSyntax.*
import io.taig.otter.schema.RecordSchema
import io.taig.otter.Merge
import cats.Invariant

trait RecordComponent[Self[_]: Invariant, -Field[_]](using self: RecordSchema[Self, Field]):
  extension [A](self: Self[A])
    final def zip[B](schema: Self[B]): Self[(A, B)] = this.self.zip(self)(schema)
    final def :*[B](field: Field[B])(using merge: Merge[A, B]): Self[merge.Out] =
      zip(this.self.lift(field)).merge

  extension [A](field: Field[A])
    def *:[B](schema: Self[B])(using merge: Merge[A, B]): Self[merge.Out] =
      this.self.lift(field).zip(schema).merge
