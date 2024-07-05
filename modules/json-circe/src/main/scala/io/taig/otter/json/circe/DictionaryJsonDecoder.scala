package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.syntax.all.*
import io.taig.otter.StringDecoder
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.Decoder

object DictionaryJsonDecoder:
  def apply[A](schema: Dictionary.Reader[A], values: Option[List[(String, Json)]]): Decoder.Result[Json, A] =
    schema match
      case Base.Dictionary.Optional(self)            => optional(self, values)
      case Base.Dictionary.Root(key, value)          => root(key, value, values)
      case Base.Dictionary.Transform(self, f, _)     => transform(self, f, values)
      case Base.Dictionary.Reader.Optional(self)     => optional(self, values)
      case Base.Dictionary.Reader.Root(key, value)   => root(key, value, values)
      case Base.Dictionary.Reader.Transform(self, f) => transform(self, f, values)

  def optional[A](self: Dictionary.Reader[A], values: Option[List[(String, Json)]]): Decoder.Result[Json, Option[A]] =
    values.fold(none.valid)(_ => DictionaryJsonDecoder(self, values).map(_.some))

  def root[A, B](
      key: Value.Required.Reader[A],
      value: Schema.Reader[B],
      values: Option[List[(String, Json)]]
  ): Decoder.Result[Json, List[(A, B)]] = values
    .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = "null".asJson)))
    .andThen(values =>
      values.traverse { case (a, b) =>
        val x: Decoder.Result[Json, A] = ??? // StringDecoder(key, a)
        val y: Decoder.Result[Json, B] = JsonDecoder(value, b)
        val z = (x, y).tupled
        z
      }
    )

  def transform[A, B](
      self: Dictionary.Reader[A],
      f: A => B,
      values: Option[List[(String, Json)]]
  ): Decoder.Result[Json, B] =
    DictionaryJsonDecoder(self, values).map(f)
