package io.taig.otter.codec

import io.taig.otter.Collection

final class CollectionEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Collection[S, *], Seq[T]]:
  override def encode[A](schema: Collection[S, A], a: A): Seq[T] = schema match
    case Collection.Indexed(schema, _) => a.map(encoder.encode(schema = schema.value, _))
    case Collection.Linked(schema, _)  => a.map(encoder.encode(schema = schema.value, _))
    case Collection.Modify(self, f, g) => encode(schema = self, g(a))

object CollectionEncoder:
  def apply[S[_], A](encoder: Encoder[S, A]): Encoder[Collection[S, *], Seq[A]] = new CollectionEncoder(encoder)
