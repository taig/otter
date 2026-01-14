package io.taig.otter.codec

import io.taig.otter.Enumeration

import scala.annotation.tailrec

final class EnumerationEncoder[F[_], A](encoder: Encoder[F, A]) extends Encoder[Enumeration.Write[F, *], A]:
  @tailrec
  override def encode[B](schema: Enumeration.Write[F, B], a: B): A = schema match
    case Enumeration.Modify(self, _, f)    => encode(schema = self, f(a))
    case Enumeration.Root(schema, mapping) => encoder.encode(schema.value, mapping.apply(a))
    case Enumeration.Write.Modify(self, f) => encode(schema = self, f(a))
