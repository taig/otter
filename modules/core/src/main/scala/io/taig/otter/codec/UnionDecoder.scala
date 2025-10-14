package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Union
import io.taig.otter.Violation
import io.taig.otter.Violations

final class UnionDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder[Union[S, *], T]:
  override def decode[A](schema: Union[S, A], value: T): Validated[Violations, A] = schema match
    case Union.Modify(self, f, _)  => decode(schema = self, value).map(f)
    case Union.OrElse(left, right) =>
      decode(schema = left, value).map(Left(_)).orElse(decode(schema = right, value).map(Right(_)))
    case Union.Root(schema) => decoder.decode(schema = schema.value, value)

object UnionDecoder:
  def apply[S[_], A](decoder: Decoder[S, A]): Decoder[Union[S, *], A] = new UnionDecoder(decoder)
