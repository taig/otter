package io.taig.otter.json

import io.taig.otter.*
import cats.data.Chain
import io.circe.Json
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated

object RecordJsonDecoder:
  def apply[A](schema: Record.Reader.Via[Json, A], values: Option[Chain[(String, Json)]]): Decoder.Result[Json, A] =
    // TODO allow to configure whether additional properties are allowed
    withRemainders(schema, values).map { case (_, a) => a }

  def withRemainders[A](
      schema: Record.Reader.Via[Json, A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], A)] = schema match
    case Record.Combine(_, left, right)        => combine(left, right, values)
    case Record.Empty(_)                       => (values, ()).valid
    case Record.One(_, field)                  => one(field, values)
    case Record.Optional(self)                 => optional(self, values)
    case Record.Reader.Combine(_, left, right) => combine(left, right, values)
    case Record.Reader.One(_, field)           => one(field, values)
    case Record.Reader.Optional(self)          => optional(self, values)
    case Record.Reader.Transform(self, f)      => transform(self, f, values)
    case Record.Transform(self, f, _)          => transform(self, f, values)

  def combine[A, B](
      left: Record.Reader.Via[Json, A],
      right: Record.Reader.Via[Json, B],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], (A, B))] = withRemainders(left, values) match
    case Validated.Valid((remainders, a)) => withRemainders(right, remainders).map(_.tupleLeft(a))
    case Validated.Invalid(violations) =>
      withRemainders(right, values).fold(violations.combine, _ => violations).invalid

  def one[A](
      field: Field.Reader.Via[Json, A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], A)] = FieldJsonDecoder(field, values)

  def optional[A](
      self: Record.Reader.Via[Json, A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], Option[A])] = values match
    case Some(values) => withRemainders(self, values.some).map(_.map(_.some))
    case None         => (values, none).valid

  def transform[A, B](
      self: Record.Reader.Via[Json, A],
      f: A => B,
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], B)] = withRemainders(self, values).map(_.map(f))
