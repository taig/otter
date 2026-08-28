package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Dictionary
import io.taig.otter.Violations
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class DictionaryDecoder[F[- _, + _], T](decoder: Decoder[F, T])
    extends Decoder[[w, r] =>> Dictionary[F, w, r], List[(String, T)]]:
  override def decode[R](
      schema: Dictionary[F, Nothing, R],
      values: List[(String, T)]
  ): Validated[Violations, R] = (schema: @unchecked) match
    case schema: Dictionary.Hashed[F, ?, ?] =>
      entries(schema.reference.value, values, schema.validation)(SortedMap.from).map(_.asInstanceOf[R])
    case schema: Dictionary.Linked[F, ?, ?] =>
      entries(schema.reference.value, values, schema.validation)(identity).map(_.asInstanceOf[R])
    case schema: Dictionary.Modify[F, ?, ?, ?, R] => decode(schema.self, values).map(schema.f)

  private def entries[R, C](
      schema: F[Nothing, R],
      values: List[(String, T)],
      validation: Validation[Constraint.Object, C]
  )(collect: List[(String, R)] => C): Validated[Violations, C] =
    values
      .traverse((key, value) => decoder.decode(schema, value).tupleLeft(key).leftMap(key /: _))
      .map(collect)
      .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
