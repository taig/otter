package io.taig.otter.component
import io.taig.otter.schema.RecordSchema
import io.taig.otter.Merge
import scala.annotation.targetName

trait RecordComponent[Self[_], -Field[_]](using self: RecordSchema[Self, Field]):
  extension [A](self: Self[A])
    @targetName("record :* field")
    def :*[B](field: Field[B])(using merge: Merge[A, B]): Self[merge.Out] = self.zip(field.toRecord).merge

  extension [A](self: Field[A])
    @targetName("field :* field")
    def :*[B](field: Field[B])(using merge: Merge[A, B]): Self[merge.Out] = self.toRecord.zip(field.toRecord).merge

    @targetName("field *: field")
    def *:[B](field: Field[B])(using merge: Merge[B, A]): Self[merge.Out] = field.toRecord.zip(self.toRecord).merge

    def toRecord: Self[A] = this.self.lift(self)
