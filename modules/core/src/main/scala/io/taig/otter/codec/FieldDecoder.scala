package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Field
import io.taig.otter.Violations
import io.taig.otter.collectFirstWithRemainders
import io.taig.validation.Violation

/** Reads a field out of the pairs a record has left to read.
  *
  * `absent` decides what counts as nothing, given what the field's key holds: `None` when there is no such key, and
  * `Some(value)` when there is one. Accepting both a missing key and the value a format writes for nothing is
  * `_.forall(isEmpty)`, accepting only the missing key is `_.isEmpty`, and accepting only the written form is
  * `_.exists(isEmpty)`. When nothing is accepted the field reads on, so a key that has to be there and is not still
  * fails the way it always has.
  */
final class FieldDecoder[F[-_, +_], T](decoder: Decoder[F, T], absent: Option[T] => Boolean)
    extends Decoder.Remaining[[w, r] =>> Field[F, w, r], Chain[(String, T)]]:
  override def decodeRemaining[R](
      field: Field[F, Nothing, R],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], R)] = field match
    case Field.Default(self, default) =>
      val (remainders, value) = take(self.name, values)

      if absent(value) then (remainders, default.value).valid else decodeRemaining(self, values)
    case Field.Modify(self, f, _) => decodeRemaining(self, values).map(_.map(f))
    case Field.Optional(self)     =>
      val (remainders, value) = take(self.name, values)

      if absent(value)
      then (remainders, None).valid
      else decodeRemaining(self, values).map(_.map(Some.apply))
    case Field.Root(name, reference) =>
      val (remainders, result) = take(name, values)

      result
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .tupleLeft(remainders)
        .leftMap(Violations.apply)
        .andThen(_.traverse(decoder.decode(reference.value, _)))
        .leftMap(name /: _)

  /** What the key holds, and what is left once it is taken. The remainders are only used where this decoder answers for
    * the key itself; where it reads on, the untouched values go down so that the field underneath takes its own.
    */
  private def take(name: String, values: Chain[(String, T)]): (Chain[(String, T)], Option[T]) =
    values.collectFirstWithRemainders { case (`name`, value) => value }
