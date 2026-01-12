package io.taig.otter.codec

import io.taig.otter.Collection

final class CollectionEncoder[F[_], A](encoder: Encoder[F, A]) extends Encoder[Collection.Write[F, *], Seq[A]]:
  override def encode[B](schema: Collection.Write[F, B], b: B): Seq[A] = schema match
    case Collection.Chained(schema, _)    => b.map(encoder.encode(schema.value, _)).toList
    case Collection.Indexed(schema, _)    => b.map(encoder.encode(schema.value, _))
    case Collection.Linked(schema, _)     => b.map(encoder.encode(schema.value, _))
    case Collection.Modify(self, _, f)    => encode(schema = self, f(b))
    case Collection.Write.Modify(self, f) => encode(schema = self, f(b))
