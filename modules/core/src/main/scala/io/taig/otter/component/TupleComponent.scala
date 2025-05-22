package io.taig.otter.component
import io.taig.otter.Merge
import io.taig.otter.schema.TupleSchema
import scala.annotation.targetName

trait TupleComponent[Self[_], -Value[_]](using self: TupleSchema[Self, Value]):
  final def TNil: Self[Unit] = self.empty

  extension [A](self: Self[A])
    @targetName("tuple :* schema")
    def :*[B](schema: Value[B])(using merge: Merge[A, B]): Self[merge.Out] = self.zip(schema.toTuple).merge

  extension [A](self: Value[A])
    @targetName("schema :* schema")
    def :*[B](schema: Value[B])(using merge: Merge[A, B]): Self[merge.Out] = self.toTuple.zip(schema.toTuple).merge

    @targetName("schema *: schema")
    def *:[B](schema: Value[B])(using merge: Merge[A, B]): Self[merge.Out] = self.toTuple.zip(schema.toTuple).merge

    @targetName("schema *: tuple")
    def *:[B](schema: Self[B])(using merge: Merge[A, B]): Self[merge.Out] = self.toTuple.zip(schema).merge

    def toTuple: Self[A] = this.self.lift(self)
