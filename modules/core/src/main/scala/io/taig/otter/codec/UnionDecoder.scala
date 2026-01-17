package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Union
import io.taig.otter.Violations

final class UnionDecoder[F[_], T](decoder: Decoder[F, T]) extends Decoder[Union.Read[F, *], T]:
  override def decode[A](schema: Union.Read[F, A], value: T): Validated[Violations, A] = schema match
    case Union.Modify(self, f, _)     => decode(schema = self, value).map(f)
    case Union.Coproduct(left, right) =>
      decode(schema = left, value).map(Left(_)).orElse(decode(schema = right, value).map(Right(_)))
    case Union.Read.Coproduct(left, right) =>
      decode(schema = left, value).map(Left(_)).orElse(decode(schema = right, value).map(Right(_)))
    case Union.Read.Modify(self, f) => decode(schema = self, value).map(f)
    case Union.Root(schema)         => decoder.decode(schema.value, value)
