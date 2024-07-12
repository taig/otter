package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.*

object DictionaryJsonDecoder:
  def apply[A](schema: Dictionary.Reader.Via[Json, A], values: Option[List[(String, Json)]]): Decoder.Result[Json, A] =
    schema match
      case Dictionary.Optional(self)             => optional(self, values)
      case Dictionary.Root(_, key, value)        => root(key, value, values)
      case Dictionary.Transform(self, f, _)      => transform(self, f, values)
      case Dictionary.Reader.Optional(self)      => optional(self, values)
      case Dictionary.Reader.Root(_, key, value) => root(key, value, values)
      case Dictionary.Reader.Transform(self, f)  => transform(self, f, values)

  def optional[A](
      self: Dictionary.Reader.Via[Json, A],
      values: Option[List[(String, Json)]]
  ): Decoder.Result[Json, Option[A]] =
    values.fold(none.valid)(_ => DictionaryJsonDecoder(self, values).map(_.some))

  def root[A, B](
      key: Value.Required.Reader.Via[String, A],
      value: Schema.Reader.Via[Json, B],
      values: Option[List[(String, Json)]]
  ): Decoder.Result[Json, List[(A, B)]] = values
    .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = "null".asJson)))
    .andThen: values =>
      values.traverse { case (a, b) =>
        (
          ValueRequiredStringDecoder(key, a).leftMap(_.bimap(_.map(_.asJson), _.asJson)),
          JsonDecoder(value, b)
        ).tupled.leftMap(a /: _)
      }

  def transform[A, B](
      self: Dictionary.Reader.Via[Json, A],
      f: A => B,
      values: Option[List[(String, Json)]]
  ): Decoder.Result[Json, B] = DictionaryJsonDecoder(self, values).map(f)
