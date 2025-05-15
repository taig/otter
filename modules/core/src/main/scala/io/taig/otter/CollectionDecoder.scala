package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

final class CollectionDecoder[S[_], T](decoder: Decoder[S, T]) extends Decoder[Collection[S, *], Seq[T]]:
  override def apply[A](schema: Collection[S, A], values: Seq[T]): Validated[Violations, A] = schema match
    case Collection.Indexed(schema, _, _, _, _) =>
      values.toVector.zipWithIndex
        .traverse((value, index) => decoder(schema = schema.value, value).leftMap(index /: _))
    case Collection.Linked(schema, _, _, _, _) =>
      values.toList.zipWithIndex
        .traverse((value, index) => decoder(schema = schema.value, value).leftMap(index /: _))
    case Collection.Modify(self, f, _) => apply(schema = self, values).map(f)
