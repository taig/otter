package io.taig.otter.codec

import io.taig.otter.Coerce

import scala.annotation.tailrec

final class CoerceEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Coerce[S, *], T]:
  override def encode[A](schema: Coerce[S, A], a: A): T = schema match
    case Coerce.Modify(self, _, g) => encode(schema = self, g(a))
    case Coerce.Root(schema)       => encoder.encode(schema = schema.value, a)

object CoerceEncoder:
  def apply[S[_], A](encoder: Encoder[S, A]): Encoder[Coerce[S, *], A] = new CoerceEncoder(encoder)
