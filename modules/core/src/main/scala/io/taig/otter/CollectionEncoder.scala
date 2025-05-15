package io.taig.otter

import scala.annotation.tailrec

final class CollectionEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Collection[S, *], Seq[T]]:
  @tailrec
  override def apply[A](schema: Collection[S, A], a: A): Seq[T] = schema match
    case Collection.Indexed(schema, _, _, _, _) => a.map(encoder(schema = schema.value, _))
    case Collection.Linked(schema, _, _, _, _)  => a.map(encoder(schema = schema.value, _))
    case Collection.Modify(self, _, g)          => apply(schema = self, g(a))
