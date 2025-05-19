package io.taig.otter.codec

import io.taig.otter.Collection

import scala.annotation.tailrec

final class CollectionEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Collection[S, *], Seq[T]]:
  @tailrec
  override def encode[A](schema: Collection[S, A], a: A): Seq[T] = schema match
    case Collection.Indexed(schema, _, _, _) => a.map(encoder.encode(schema = schema.value, _))
    case Collection.Linked(schema, _, _, _)  => a.map(encoder.encode(schema = schema.value, _))
    case Collection.Modify(self, _, g)       => encode(schema = self, g(a))
