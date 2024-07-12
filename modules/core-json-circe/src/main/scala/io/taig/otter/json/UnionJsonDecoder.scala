package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*
import cats.syntax.all.*

// TODO branch / namespace, as the errors are otherwise hard to track, perhaps this
// can / should be incorporated into Sum?
// actually, using an index might do?
object UnionJsonDecoder:
  def apply[A](schema: Union.Reader.Via[Json, A], json: Json): Decoder.Result[Json, A] = schema match
    case Union.Combine(_, left, right)                       => combine(left, right, json)
    case Union.Optional(self)                                => optional(self, json)
    case Union.Reader.Combine(_, left, right)                => combine(left, right, json)
    case Union.Reader.Optional(self)                         => optional(self, json)
    case Union.Reader.Root(_, schema)                        => root(schema, json)
    case Union.Reader.Transform(self, f)                     => transform(self, f, json)
    case Union.Root(_, schema)                               => root(schema, json)
    case Union.Transform(self, f, _)                         => transform(self, f, json)
    case Union.Value.Combine(_, left, right)                 => combine(left, right, json)
    case Union.Value.Optional(self)                          => optional(self, json)
    case Union.Value.Reader.Combine(_, left, right)          => combine(left, right, json)
    case Union.Value.Reader.Optional(self)                   => optional(self, json)
    case Union.Value.Reader.Transform(self, f)               => transform(self, f, json)
    case Union.Value.Required.Combine(_, left, right)        => combine(left, right, json)
    case Union.Value.Required.Reader.Combine(_, left, right) => combine(left, right, json)
    case Union.Value.Required.Reader.Root(_, schema)         => root(schema, json)
    case Union.Value.Required.Reader.Transform(self, f)      => transform(self, f, json)
    case Union.Value.Required.Transform(self, f, _)          => transform(self, f, json)
    case Union.Value.Transform(self, f, _)                   => transform(self, f, json)

  def combine[A, B](
      left: Union.Reader.Via[Json, A],
      right: Union.Reader.Via[Json, B],
      json: Json
  ): Decoder.Result[Json, Either[A, B]] =
    UnionJsonDecoder(left, json).map(_.asLeft).orElse(UnionJsonDecoder(right, json).map(_.asRight))

  def optional[A](self: Union.Reader.Via[Json, A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else apply(self, json).map(_.some)

  def root[A](schema: Schema.Reader.Via[Json, A], json: Json): Decoder.Result[Json, A] =
    JsonDecoder(schema, json)

  def transform[A, B](schema: Union.Reader.Via[Json, A], f: A => B, json: Json): Decoder.Result[Json, B] =
    apply(schema, json).map(f)
