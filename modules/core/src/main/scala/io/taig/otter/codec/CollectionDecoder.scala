package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Collection
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.validation.Validation

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class CollectionDecoder[F[-_, +_], T](decoder: Decoder[F, T])
    extends Decoder[[w, r] =>> Collection[F, w, r], Seq[T]]:
  override def decode[R](schema: Collection[F, Nothing, R], values: Seq[T]): Validated[Violations, R] =
    (schema: @unchecked) match
      case schema: Collection.Chained[F, ?, ?] =>
        elements(schema.reference.value, values, schema.validation)(Chain.fromSeq).map(_.asInstanceOf[R])
      case schema: Collection.Indexed[F, ?, ?] =>
        elements(schema.reference.value, values, schema.validation)(_.toVector).map(_.asInstanceOf[R])
      case schema: Collection.Linked[F, ?, ?] =>
        elements(schema.reference.value, values, schema.validation)(_.toList).map(_.asInstanceOf[R])
      case schema: Collection.Modify[F, ?, ?, ?, R] => decode(schema.self, values).map(schema.f)

  private def elements[R, C](
      schema: F[Nothing, R],
      values: Seq[T],
      validation: Validation[Constraint.Collection, C]
  )(collect: Seq[R] => C): Validated[Violations, C] =
    values.zipWithIndex
      .traverse((value, index) => decoder.decode(schema, value).leftMap(index /: _))
      .map(collect)
      .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
