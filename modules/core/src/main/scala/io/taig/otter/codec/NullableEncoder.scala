package io.taig.otter.codec

import io.taig.otter.Nullable
import io.taig.otter.Nullable.Default
import io.taig.otter.Nullable.Modify
import io.taig.otter.Nullable.Optional

import scala.annotation.tailrec

final class NullableEncoder[-S[_], T](encoder: Encoder[S, T], empty: T) extends Encoder[Nullable[S, *], T]:
  @tailrec
  override def encode[A](schema: Nullable[S, A], a: A): T = schema match
    case Nullable.Default(schema, _) => encoder.encode(schema = schema.value, a)
    case Nullable.Modify(self, _, g) => encode(schema = self, g(a))
    case Nullable.Optional(schema)   => a.fold(empty)(encoder.encode(schema = schema.value, _))

object NullableEncoder:
  def apply[S[_], A](encoder: Encoder[S, A], empty: A): NullableEncoder[S, A] =
    new NullableEncoder(encoder, empty)
