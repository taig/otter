package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Coerce
import io.taig.otter.Violations

final class CoerceDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder[Coerce[S, *], T]:
  override def decode[A](schema: Coerce[S, A], value: T): Validated[Violations, A] = schema match
    case Coerce.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Coerce.Root(schema)       => decoder.decode(schema = schema.value, value)

object CoerceDecoder:
  def apply[S[_], A](decoder: Decoder[S, A]): Decoder[Coerce[S, *], A] = new CoerceDecoder(decoder)
