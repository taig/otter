package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*

object JsonCollectionEncoder:
  def apply[A](schema: Collection.Writer[A], a: A): Option[Vector[Json]] = schema match
    // case Base.Collection.Invariant(self, _, f)         => contravariant(self, f, a)
    case Base.Collection.Optional(self)                => optional(self, a)
    case Base.Collection.Root(_, schema)               => root(schema, a)
    case Base.Collection.Writer.Contravariant(self, f) => contravariant(self, f, a)
    case Base.Collection.Writer.Optional(self)         => optional(self, a)
    case Base.Collection.Writer.Root(_, schema)        => root(schema, a)

  def contravariant[A, B](self: Collection.Writer[A], f: B => A, b: B): Option[Vector[Json]] = apply(self, f(b))

  def optional[A](self: Collection.Writer[A], a: Option[A]): Option[Vector[Json]] = a.flatMap(apply(self, _))

  def root[A](schema: Schema.Writer[A], a: Vector[A]): Option[Vector[Json]] = a.map(JsonEncoder(schema, _)).some
