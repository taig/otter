package io.taig.otter.codec

import io.taig.otter.Enumeration

import scala.annotation.tailrec

final class EnumerationEncoder[F[_], T](encoder: Encoder[F, T]) extends Encoder[Enumeration.Write[F, *], T]:
  @tailrec
  override def encode[A](schema: Enumeration.Write[F, A], a: A): T = schema match
    case Enumeration.Modify(self, _, f)    => encode(schema = self, f(a))
    case Enumeration.Root(schema, mapping) => encoder.encode(schema.value, mapping.apply(a))
    case Enumeration.Write.Modify(self, f) => encode(schema = self, f(a))
