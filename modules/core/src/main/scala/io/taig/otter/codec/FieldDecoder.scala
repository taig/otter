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

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class FieldDecoder[F[-_, +_], T](decoder: Decoder[F, T])
    extends Decoder.Remaining[[w, r] =>> Field[F, w, r], Chain[(String, T)]]:
  override def decodeRemaining[R](
      field: Field[F, Nothing, R],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], R)] = (field: @unchecked) match
    case field: Field.Default[F, ?, R] =>
      if values.exists((key, _) => key === field.name)
      then decodeRemaining(field.self, values)
      else (values, field.value.value).valid
    case field: Field.Modify[F, ?, ?, ?, R] => decodeRemaining(field.self, values).map(_.map(field.f))
    case field: Field.Optional[F, ?, ?]     =>
      if values.exists((key, _) => key === field.name)
      then decodeRemaining(field.self, values).map(_.map(_.some.asInstanceOf[R]))
      else (values, none.asInstanceOf[R]).valid
    case field: Field.Root[F, ?, R] =>
      val name = field.name
      val (remainders, result) = values.collectFirstWithRemainders { case (`name`, value) => value }

      result
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .tupleLeft(remainders)
        .leftMap(Violations.apply)
        .andThen(_.traverse(decoder.decode(field.reference.value, _)))
        .leftMap(name /: _)
