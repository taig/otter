package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Collection
import io.taig.otter.Violations

final class CollectionDecoder[S[_], T](decoder: Decoder[S, T]) extends Decoder[Collection[S, *], Seq[T]]:
  override def decode[A](schema: Collection[S, A], values: Seq[T]): Validated[Violations, A] = schema match
    case Collection.Indexed(schema, _, _, _, _) =>
      values.toVector.zipWithIndex
        .traverse((value, index) => decoder.decode(schema = schema.value, value).leftMap(index /: _))
    case Collection.Linked(schema, _, _, _, _) =>
      values.toList.zipWithIndex
        .traverse((value, index) => decoder.decode(schema = schema.value, value).leftMap(index /: _))
    case Collection.Modify(self, f, _) => decode(schema = self, values).map(f)
