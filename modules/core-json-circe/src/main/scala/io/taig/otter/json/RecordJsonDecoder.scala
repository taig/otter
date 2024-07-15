package io.taig.otter.json

import io.taig.otter.*
import cats.data.Chain
import io.circe.Json
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated

object RecordJsonDecoder:
  def apply[A](schema: Record[?, A], values: Option[Chain[(String, Json)]]): Decoder.Result[Data, A] =
    // TODO allow to configure whether additional properties are allowed
    withRemainders(schema, values).map { case (_, a) => a }

  def withRemainders[A](
      schema: Record[?, A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Data, (Option[Chain[(String, Json)]], A)] = schema match
    case Record.Combine(_, left, right) =>
      withRemainders(left, values) match
        case Validated.Valid((remainders, a)) => withRemainders(right, remainders).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          withRemainders(right, values).fold(violations.combine, _ => violations).invalid
    case Record.Empty(_)      => (values, ()).valid
    case Record.One(_, field) => FieldJsonDecoder(field, values)
    case Record.Optional(self) =>
      values match
        case Some(values) => withRemainders(self, values.some).map(_.map(_.some))
        case None         => (values, none).valid
    case Record.Transform(self, f, _) => withRemainders(self, values).map(_.map(f))
