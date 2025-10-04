package io.taig.otter.codec

import io.taig.otter.Enumeration
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

final class EnumerationEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Enumeration[S, *], T]:
  override def encode[A](schema: Enumeration[S, A], a: A): T = schema match
    case Enumeration.Modify(self, _, g)    => encode(schema = self, g(a))
    case Enumeration.Root(schema, mapping) => encoder.encode(schema = schema.value, mapping.apply(a))

object EnumerationEncoder:
  def apply[S[_], A](encoder: Encoder[S, A]): Encoder[Enumeration[S, *], A] = new EnumerationEncoder(encoder)
