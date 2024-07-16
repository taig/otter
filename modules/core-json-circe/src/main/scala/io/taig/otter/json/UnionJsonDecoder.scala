package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*
import cats.syntax.all.*

// TODO branch / namespace, as the errors are otherwise hard to track, perhaps this
// can / should be incorporated into Sum?
// actually, using an index might do?
object UnionJsonDecoder:
  def apply[A](schema: Union[?, A], json: Json): Decoder.Result[Data, A] = schema match
    case Union.Combine(_, left, right)                => combine(left, right, json)
    case Union.Optional(self)                         => optional(self, json)
    case Union.Root(_, schema)                        => root(schema, json)
    case Union.Transform(self, f, _)                  => transform(self, f, json)
    case Union.Value.Combine(_, left, right)          => combine(left, right, json)
    case Union.Value.Optional(self)                   => optional(self, json)
    case Union.Value.Required.Combine(_, left, right) => combine(left, right, json)
    case Union.Value.Required.Transform(self, f, _)   => transform(self, f, json)
    case Union.Value.Transform(self, f, _)            => transform(self, f, json)

  def combine[A, B](
      left: Union[?, A],
      right: Union[?, B],
      json: Json
  ): Decoder.Result[Data, Either[A, B]] =
    UnionJsonDecoder(left, json).map(_.asLeft).orElse(UnionJsonDecoder(right, json).map(_.asRight))

  def optional[A](self: Union[?, A], json: Json): Decoder.Result[Data, Option[A]] =
    if json.isNull then none.valid else apply(self, json).map(_.some)

  def root[A](schema: Codec[?, A], json: Json): Decoder.Result[Data, A] =
    JsonDecoder(schema, json)

  def transform[A, B](schema: Union[?, A], f: A => B, json: Json): Decoder.Result[Data, B] =
    apply(schema, json).map(f)
