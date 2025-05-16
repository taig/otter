package io.taig.otter.codec

import io.taig.otter.Enumeration
import scala.annotation.tailrec

final class EnumerationEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Enumeration[S, *], T]:
  @tailrec
  override def apply[A](schema: Enumeration[S, A], a: A): T = schema match
    case Enumeration.Modify(self, _, g) =>  apply(schema = self, g(a))
    case Enumeration.Root(schema, mapping, _) => encoder(schema = schema.value, mapping(a))
  