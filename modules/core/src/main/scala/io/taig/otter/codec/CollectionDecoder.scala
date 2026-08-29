package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Collection
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.validation.Validation

final class CollectionDecoder[F[-_, +_], T](decoder: Decoder[F, T])
    extends Decoder[[w, r] =>> Collection[F, w, r], Seq[T]]:
  override def decode[R](schema: Collection[F, Nothing, R], values: Seq[T]): Validated[Violations, R] =
    schema match
      case Collection.Chained(reference, validation) =>
        elements(reference.value, values, validation)(Chain.fromSeq)
      case Collection.Indexed(reference, validation) => elements(reference.value, values, validation)(_.toVector)
      case Collection.Linked(reference, validation)  => elements(reference.value, values, validation)(_.toList)
      case Collection.Modify(self, f, _)             => decode(self, values).map(f)

  private def elements[R, C](
      schema: F[Nothing, R],
      values: Seq[T],
      validation: Validation[Constraint.Collection, C]
  )(collect: Seq[R] => C): Validated[Violations, C] =
    values.zipWithIndex
      .traverse((value, index) => decoder.decode(schema, value).leftMap(index /: _))
      .map(collect)
      .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
