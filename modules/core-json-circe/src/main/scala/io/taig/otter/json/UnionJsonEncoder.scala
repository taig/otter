package io.taig.otter.json

import io.taig.otter.*
import io.circe.Json

object UnionJsonEncoder:
  def apply[A](schema: Union[?, A], a: A): Json = schema match
    case Union.Combine(_, left, right)                => combine(left, right, a)
    case Union.Optional(self)                         => optional(self, a)
    case Union.Root(_, schema)                        => root(schema, a)
    case Union.Transform(self, _, f)                  => transform(self, f, a)
    case Union.Value.Combine(_, left, right)          => combine(left, right, a)
    case Union.Value.Optional(self)                   => optional(self, a)
    case Union.Value.Required.Combine(_, left, right) => combine(left, right, a)
    case Union.Value.Required.Transform(self, _, f)   => transform(self, f, a)
    case Union.Value.Transform(self, _, f)            => transform(self, f, a)

  def combine[A, B](left: Union[?, A], right: Schema[?, B], ab: Either[A, B]): Json =
    ab.fold(apply(left, _), JsonEncoder(right, _))

  def optional[A](self: Union[?, A], a: Option[A]): Json = a.map(apply(self, _)).getOrElse(Json.Null)

  def root[A](schema: Schema[?, A], a: A): Json = JsonEncoder(schema, a)

  def transform[A, B](self: Union[?, A], f: B => A, b: B): Json = apply(self, f(b))
