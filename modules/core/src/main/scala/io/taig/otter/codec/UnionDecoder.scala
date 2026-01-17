package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Union
import io.taig.otter.Violations

final class UnionDecoder[F[_], A](decoder: Decoder[F, A]) extends Decoder[Union.Read[F, *], A]:
  override def decode[B](schema: Union.Read[F, B], value: A): Validated[Violations, B] = schema match
    case Union.Modify(self, f, _)     => decode(schema = self, value).map(f)
    case Union.Coproduct(left, right) =>
      decode(schema = left, value).map(Left(_)).orElse(decode(schema = right, value).map(Right(_)))
    case Union.Read.Coproduct(left, right) =>
      decode(schema = left, value).map(Left(_)).orElse(decode(schema = right, value).map(Right(_)))
    case Union.Read.Modify(self, f) => decode(schema = self, value).map(f)
    case Union.Root(schema)         => decoder.decode(schema.value, value)
