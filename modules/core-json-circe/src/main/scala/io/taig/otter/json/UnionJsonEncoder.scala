package io.taig.otter.json

import io.taig.otter.+
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json

object UnionJsonEncoder:
  def apply[A](schema: Union.Writer[A], a: A): Json = schema match
    case Base.Union.Combine(left, right)                       => combine(left, right, a)
    case Base.Union.Optional(self)                             => optional(self, a)
    case Base.Union.Root(schema)                               => root(schema, a)
    case Base.Union.Transform(self, _, f)                      => transform(self, f, a)
    case Base.Union.Value.Combine(left, right)                 => combine(left, right, a)
    case Base.Union.Value.Optional(self)                       => optional(self, a)
    case Base.Union.Value.Required.Combine(left, right)        => combine(left, right, a)
    case Base.Union.Value.Required.Transform(self, _, f)       => transform(self, f, a)
    case Base.Union.Value.Required.Writer.Combine(left, right) => combine(left, right, a)
    case Base.Union.Value.Required.Writer.Root(schema)         => root(schema, a)
    case Base.Union.Value.Required.Writer.Transform(self, f)   => transform(self, f, a)
    case Base.Union.Value.Transform(self, _, f)                => transform(self, f, a)
    case Base.Union.Value.Writer.Combine(left, right)          => combine(left, right, a)
    case Base.Union.Value.Writer.Optional(self)                => optional(self, a)
    case Base.Union.Value.Writer.Transform(self, f)            => transform(self, f, a)
    case Base.Union.Writer.Combine(left, right)                => combine(left, right, a)
    case Base.Union.Writer.Optional(self)                      => optional(self, a)
    case Base.Union.Writer.Root(schema)                        => root(schema, a)
    case Base.Union.Writer.Transform(self, f)                  => transform(self, f, a)

  def combine[A, B](left: Union.Writer[A], right: Schema.Writer[B], ab: A + B): Json =
    ab.fold(apply(left, _), JsonEncoder(right, _))

  def optional[A](self: Union.Writer[A], a: Option[A]): Json = a.map(apply(self, _)).getOrElse(Json.Null)

  def root[A](schema: Schema.Writer[A], a: A): Json = JsonEncoder(schema, a)

  def transform[A, B](self: Union.Writer[A], f: B => A, b: B): Json = apply(self, f(b))
