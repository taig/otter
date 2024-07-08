package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.data.Chain
import io.circe.Json
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated

object RecordJsonDecoder:
  def apply[A](schema: Record.Reader[A], values: Option[Chain[(String, Json)]]): Decoder.Result[Json, A] =
    // TODO allow to configure whether additional properties are allowed
    withRemainders(schema, values).map { case (_, a) => a }

  def withRemainders[A](
      schema: Record.Reader[A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], A)] = schema match
    case Base.Record.Combine(left, right)      => combine(left, right, values)
    case Base.Record.Empty                     => (values, ()).valid
    case Base.Record.Nulls(self, _)            => withRemainders(self, values)
    case Base.Record.One(field)                => one(field, values)
    case Base.Record.Optional(self)            => optional(self, values)
    case Base.Record.Reader.One(field)         => one(field, values)
    case Base.Record.Reader.Optional(self)     => optional(self, values)
    case Base.Record.Reader.Transform(self, f) => transform(self, f, values)
    case Base.Record.Transform(self, f, _)     => transform(self, f, values)

  def combine[A, B](
      left: Record.Reader[A],
      right: Record.Reader[B],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], (A, B))] = withRemainders(left, values) match
    case Validated.Valid((remainders, a)) => withRemainders(right, remainders).map(_.tupleLeft(a))
    case Validated.Invalid(violations) =>
      withRemainders(right, values).fold(violations.combine, _ => violations).invalid

  def one[A](
      field: Field.Reader[A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], A)] = FieldJsonDecoder(field, values)

  def optional[A](
      self: Record.Reader[A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], Option[A])] = values match
    case Some(values) => withRemainders(self, values.some).map(_.map(_.some))
    case None         => (values, none).valid

  def transform[A, B](
      self: Record.Reader[A],
      f: A => B,
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], B)] = withRemainders(self, values).map(_.map(f))
