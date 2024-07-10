package io.taig.otter.json

import io.circe.Json
import io.taig.otter.Decoder
import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.syntax.all.*

// TODO branch / namespace, as the errors are otherwise hard to track, perhaps this
// can / should be incorporated into Sum?
// actually, using an index might do?
object UnionJsonDecoder:
  def apply[A](schema: Union.Reader.Via[Json, A], json: Json): Decoder.Result[Json, A] = schema match
    case Base.Union.Combine(left, right)                       => combine(left, right, json)
    case Base.Union.Optional(self)                             => optional(self, json)
    case Base.Union.Reader.Combine(left, right)                => combine(left, right, json)
    case Base.Union.Reader.Optional(self)                      => optional(self, json)
    case Base.Union.Reader.Root(schema)                        => root(schema, json)
    case Base.Union.Reader.Transform(self, f)                  => transform(self, f, json)
    case Base.Union.Root(schema)                               => root(schema, json)
    case Base.Union.Transform(self, f, _)                      => transform(self, f, json)
    case Base.Union.Value.Combine(left, right)                 => combine(left, right, json)
    case Base.Union.Value.Optional(self)                       => optional(self, json)
    case Base.Union.Value.Reader.Combine(left, right)          => combine(left, right, json)
    case Base.Union.Value.Reader.Optional(self)                => optional(self, json)
    case Base.Union.Value.Reader.Transform(self, f)            => transform(self, f, json)
    case Base.Union.Value.Required.Combine(left, right)        => combine(left, right, json)
    case Base.Union.Value.Required.Reader.Combine(left, right) => combine(left, right, json)
    case Base.Union.Value.Required.Reader.Root(schema)         => root(schema, json)
    case Base.Union.Value.Required.Reader.Transform(self, f)   => transform(self, f, json)
    case Base.Union.Value.Required.Transform(self, f, _)       => transform(self, f, json)
    case Base.Union.Value.Transform(self, f, _)                => transform(self, f, json)

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
