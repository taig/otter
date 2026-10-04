package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Field
import io.taig.otter.Violations
import io.taig.validation.Violation

/** Reads a field once, then lets its structural contract distinguish missing from format-empty values. */
final class FieldDecoder[F[-_, +_], T](decoder: Decoder[F, T], isEmpty: T => Boolean)
    extends Decoder.Remaining[Field[F, *, *], Fields[T]]:
  override def decodeRemaining[R](
      field: Field[F, Nothing, R],
      values: Fields[T]
  ): Validated[Violations, (Fields[T], R)] =
    val (remainders, value) = values.take(field.name)

    decode(field, value).tupleLeft(remainders)

  private def decode[R](field: Field[F, Nothing, R], value: Option[T]): Validated[Violations, R] = field match
    case Field.Default(self, default, absent) =>
      if absent.matches(value, isEmpty) then default.value.valid else decode(self, value)
    case Field.Modify(self, f, _)       => decode(self, value).map(f)
    case Field.Optional(self, presence) =>
      if presence.absent.matches(value, isEmpty) then Validated.valid(None) else decode(self, value).map(Some.apply)
    case Field.Root(name, reference) =>
      value
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .leftMap(Violations.apply)
        .andThen(decoder.decode(reference.value, _))
        .leftMap(name /: _)
