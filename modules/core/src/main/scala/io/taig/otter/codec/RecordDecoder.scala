package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.Record
import io.taig.otter.Violations
import io.taig.validation.Violation
import io.taig.otter.Constraint

final class RecordDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder.Remaining[Record[S, *], Chain[(String, T)]]:
  override def decodeRemaining[A](
      schema: Record[S, A],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], A)] = schema match
    case Record.Default(self, default) =>
      val keys = Set.from(values.map((key, _) => key).toIterable)
      if self.fields.exists(field => keys.contains(field.name))
      then decodeRemaining(schema = self, values)
      else (values, default.value).valid
    case Record.Empty              => (values, ()).valid
    case Record.Modify(self, f, _) => decodeRemaining(schema = self, values).map(_.map(f))
    case Record.Optional(self)     =>
      val keys = Set.from(values.map((key, _) => key).toIterable)
      if self.fields.exists(field => keys.contains(field.name))
      then decodeRemaining(schema = self, values).map(_.map(_.some))
      else (values, none).valid
    case Record.Root(field) =>
      val (remainders, result) = values.collectFirstWithRemainders { case (field.name, value) => value }

      result
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .leftMap(Violations.apply)
        .andThen(decoder.decode(schema = field.schema.value, _).tupleLeft(remainders))
    case Record.Zip(left, right) =>
      decodeRemaining(schema = left, values) match
        case Validated.Valid((values, a)) =>
          decodeRemaining(schema = right, values) match
            case Validated.Valid((values, b))  => (values, (a, b)).valid
            case result @ Validated.Invalid(_) => result
        case result @ Validated.Invalid(left) =>
          decodeRemaining(schema = right, values) match
            case Validated.Valid(_)       => result
            case Validated.Invalid(right) => Validated.Invalid(left |+| right)

object RecordDecoder:
  def apply[S[_], T](decoder: Decoder.Remaining[S, T]): Decoder.Remaining[Record[S, *], Chain[(String, T)]] =
    new RecordDecoder(decoder)
