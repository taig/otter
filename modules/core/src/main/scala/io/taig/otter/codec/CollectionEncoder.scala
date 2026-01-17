package io.taig.otter.codec

import io.taig.otter.Collection

final class CollectionEncoder[F[_], T](encoder: Encoder[F, T]) extends Encoder[Collection.Write[F, *], Seq[T]]:
  override def encode[A](schema: Collection.Write[F, A], a: A): Seq[T] = schema match
    case Collection.Chained(schema, _)    => a.map(encoder.encode(schema.value, _)).toList
    case Collection.Indexed(schema, _)    => a.map(encoder.encode(schema.value, _))
    case Collection.Linked(schema, _)     => a.map(encoder.encode(schema.value, _))
    case Collection.Modify(self, _, f)    => encode(schema = self, f(a))
    case Collection.Write.Modify(self, f) => encode(schema = self, f(a))
