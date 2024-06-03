package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*

object JsonCollectionEncoder:
  def apply[A](schema: Collection.Writer[A], a: A): Option[Vector[Json]] = schema match
    case Base.Collection.Modify(self, _, f)     => modify(self, f, a)
    case Base.Collection.Optional(self)         => optional(self, a)
    case Base.Collection.Root(schema)           => root(schema, a)
    case Base.Collection.Writer.Modify(self, f) => modify(self, f, a)
    case Base.Collection.Writer.Optional(self)  => optional(self, a)
    case Base.Collection.Writer.Root(schema)    => root(schema, a)

  def modify[A, B](self: Collection.Writer[A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))

  def optional[A](self: Collection.Writer[A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

  def root[A](schema: Schema.Writer[A], a: Vector[A]): Option[Vector[Json]] = a.map(JsonEncoder(schema, _)).some
