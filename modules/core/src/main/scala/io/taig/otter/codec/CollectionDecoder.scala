package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Collection
import io.taig.otter.Violations
import cats.data.Chain

final class CollectionDecoder[F[_], A](decoder: Decoder[F, A]) extends Decoder[Collection.Read[F, *], Seq[A]]:
  override def decode[B](schema: Collection.Read[F, B], values: Seq[A]): Validated[Violations, B] = schema match
    case Collection.Chained(schema, validation) =>
      values.zipWithIndex
        .traverse((value, index) => decoder.decode(schema.value, value).leftMap(index /: _))
        .map(Chain.fromSeq)
        .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
    case Collection.Indexed(schema, validation) =>
      values.zipWithIndex
        .traverse((value, index) => decoder.decode(schema.value, value).leftMap(index /: _))
        .map(_.toVector)
        .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
    case Collection.Linked(schema, validation) =>
      values.zipWithIndex
        .traverse((value, index) => decoder.decode(schema.value, value).leftMap(index /: _))
        .map(_.toList)
        .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
    case Collection.Modify(self, f, _)   => decode(schema = self, values).map(f)
    case Collection.Read.Modify(self, f) => decode(schema = self, values).map(f)
