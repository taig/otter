package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Field
import io.taig.otter.Violations
import io.taig.validation.Violation

/** Reads a field out of the values a record has left to read.
  *
  * `absent` decides what counts as nothing, given what the field's key holds: `None` when there is no such key, and
  * `Some(value)` when there is one. Accepting both a missing key and the value a format writes for nothing is
  * `_.forall(isEmpty)`, accepting only the missing key is `_.isEmpty`, and accepting only the written form is
  * `_.exists(isEmpty)`. When nothing is accepted the field reads on, so a key that has to be there and is not still
  * fails the way it always has.
  *
  * A field is a stack of [[Field.Default]], [[Field.Optional]] and [[Field.Modify]] over exactly one [[Field.Root]],
  * and every one of them exports that root's name, so the key is taken once here and what it held is handed down.
  */
final class FieldDecoder[F[-_, +_], T](decoder: Decoder[F, T], absent: Option[T] => Boolean)
    extends Decoder.Remaining[[w, r] =>> Field[F, w, r], Fields[T]]:
  override def decodeRemaining[R](
      field: Field[F, Nothing, R],
      values: Fields[T]
  ): Validated[Violations, (Fields[T], R)] =
    val (remainders, value) = values.take(field.name)

    decode(field, value).tupleLeft(remainders)

  private def decode[R](field: Field[F, Nothing, R], value: Option[T]): Validated[Violations, R] = field match
    case Field.Default(self, default) => if absent(value) then default.value.valid else decode(self, value)
    case Field.Modify(self, f, _)     => decode(self, value).map(f)
    case Field.Optional(self)         =>
      if absent(value) then Validated.valid(None) else decode(self, value).map(Some.apply)
    case Field.Root(name, reference) =>
      value
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .leftMap(Violations.apply)
        .andThen(decoder.decode(reference.value, _))
        .leftMap(name /: _)
