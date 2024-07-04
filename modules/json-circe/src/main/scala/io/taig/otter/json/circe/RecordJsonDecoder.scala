package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.data.Chain
import io.circe.Json
import io.taig.otter.Decoder
import cats.syntax.all.*

object RecordJsonDecoder:
  def apply[A](schema: Record.Reader[A], values: Option[Chain[(String, Json)]]): Decoder.Result[Json, A] =
    // TODO allow to configure whether additional properties are allowed
    withRemainders(schema, values)._2

  def withRemainders[A](
      schema: Record.Reader[A],
      values: Option[Chain[(String, Json)]]
  ): (Option[Chain[(String, Json)]], Decoder.Result[Json, A]) = schema match
    case Base.Record.Combine(left, right)      => ???
    case Base.Record.Empty                     => (values, ().valid)
    case Base.Record.One(field)                => one(field, values)
    case Base.Record.Optional(self)            => ???
    case Base.Record.Reader.One(field)         => one(field, values)
    case Base.Record.Reader.Optional(self)     => ???
    case Base.Record.Reader.Transform(self, f) => ???
    case Base.Record.Transform(self, _, f)     => ???

  def one[A](
      field: Field.Reader[A],
      values: Option[Chain[(String, Json)]]
  ): (Option[Chain[(String, Json)]], Decoder.Result[Json, A]) = FieldJsonDecoder(field, values)
