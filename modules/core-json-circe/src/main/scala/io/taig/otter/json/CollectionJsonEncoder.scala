package io.taig.otter.json

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*

object CollectionJsonEncoder:
  def apply[A](schema: Collection.Writer.Via[Json, A], a: A): Option[Vector[Json]] = schema match
    case Base.Collection.Optional(self)            => optional(self, a)
    case Base.Collection.Root(schema)              => root(schema, a).some
    case Base.Collection.Transform(self, _, f)     => transform(self, f, a)
    case Base.Collection.Writer.Optional(self)     => optional(self, a)
    case Base.Collection.Writer.Root(schema)       => root(schema, a).some
    case Base.Collection.Writer.Transform(self, f) => transform(self, f, a)

  def optional[A](self: Collection.Writer.Via[Json, A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

  def root[A](schema: Schema.Writer.Via[Json, A], a: Vector[A]): Vector[Json] = a.map(JsonEncoder(schema, _))

  def transform[A, B](self: Collection.Writer.Via[Json, A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))
