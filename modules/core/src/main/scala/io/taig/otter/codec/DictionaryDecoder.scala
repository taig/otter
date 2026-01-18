package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Dictionary
import io.taig.otter.Violations

import scala.collection.immutable.SortedMap

final class DictionaryDecoder[F[_], T](decoder: Decoder[F, T])
    extends Decoder[Dictionary.Read[F, *], List[(String, T)]]:
  override def decode[A](schema: Dictionary.Read[F, A], values: List[(String, T)]): Validated[Violations, A] =
    schema match
      case Dictionary.Hashed(schema, validation) =>
        values
          .traverse((key, value) => decoder.decode(schema.value, value).tupleLeft(key).leftMap(key /: _))
          .map(SortedMap.from)
          .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
      case Dictionary.Linked(schema, validation) =>
        values
          .traverse((key, value) => decoder.decode(schema.value, value).tupleLeft(key).leftMap(key /: _))
          .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
      case Dictionary.Modify(self, f, _)   => decode(schema = self, values).map(f)
      case Dictionary.Read.Modify(self, f) => decode(schema = self, values).map(f)
