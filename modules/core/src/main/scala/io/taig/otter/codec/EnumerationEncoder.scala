package io.taig.otter.codec

import io.taig.otter.Enumeration

import scala.annotation.tailrec

final class EnumerationEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Enumeration[S, *], T]:
  override def encode[A](schema: Enumeration[S, A], a: A): T = encode(schema = schema.value, a)

  @tailrec
  def encode[A](schema: Enumeration.Value[S, A], a: A): T = schema match
    case Enumeration.Value.Modify(self, _, g)    => encode(schema = self, g(a))
    case Enumeration.Value.Root(schema, mapping) => encoder.encode(schema = schema.value, mapping(a))
