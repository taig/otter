package io.taig.otter.component
import io.taig.otter.Merge
import io.taig.otter.schema.TupleSchema

trait TupleComponent[Self[_], -Value[_]](using self: TupleSchema[Self, Value]):
  final def TNil: Self[Unit] = self.empty

  extension [A](self: Value[A])
    def :*[B](schema: Value[B])(using merge: Merge[A, B]): Self[merge.Out] = self.toTuple.zip(schema.toTuple).merge

    def *:[B](schema: Value[B])(using merge: Merge[B, A]): Self[merge.Out] = schema.toTuple.zip(self.toTuple).merge

    def toTuple: Self[A] = this.self.lift(self)
