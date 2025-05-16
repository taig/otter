package io.taig.otter.component

import io.taig.otter.schema.UnionSchema

import scala.annotation.targetName

trait UnionComponent[Self[_], Value[_]](using self: UnionSchema[Self, Value]):
  extension [A](self: Self[A])
    @targetName("unionOrElse")
    def orElse[B](schema: Self[B]): Self[Either[A, B]] = this.self.orElse(self)(schema)

    @targetName("unionAppend")
    def :+[B](schema: Value[B]): Self[Either[A, B]] = orElse(schema.toUnion)

  extension [A](value: Value[A])
    @targetName("unionPrepend")
    def +:[B](schema: Self[B]): Self[Either[A, B]] = value.toUnion.orElse(schema)

    def toUnion: Self[A] = self.lift(value)

  given [A]: Conversion[Value[A], Self[A]] = self.lift
