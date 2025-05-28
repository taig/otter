package io.taig.otter.codec

import io.taig.otter.Collection

import scala.annotation.tailrec

final class CollectionEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Collection[S, *], Seq[T]]:
  override def encode[A](schema: Collection[S, A], a: A): Seq[T] = encode(schema = schema.value, a)

  @tailrec
  def encode[A](schema: Collection.Value[S, A], a: A): Seq[T] = schema match
    case Collection.Value.Indexed(schema, _, _, _) => a.map(encoder.encode(schema = schema.value, _))
    case Collection.Value.Linked(schema, _, _, _)  => a.map(encoder.encode(schema = schema.value, _))
    case Collection.Value.Modify(self, _, g)       => encode(schema = self, g(a))
