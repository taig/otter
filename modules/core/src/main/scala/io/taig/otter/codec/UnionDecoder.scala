package io.taig.otter.codec

import io.taig.otter.Union
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*

final class UnionDecoder[S[_], T](decoder: Decoder[S, T]) extends Decoder[Union[S, *], T]:
  override def decode[A](schema: Union[S, A], value: T): Validated[Violations, A] =
    decode(schema, value, index = schema.schemas.length - 1)

  def decode[A](schema: Union[S, A], value: T, index: Long): Validated[Violations, A] = schema match
    case Union.OrElse(left, right, _) =>
      decode(schema = left, value, index = index - 1)
        .map(Left(_))
        .findValid(decode(schema = right, value, index).map(Right(_)))
    case Union.Root(schema, _)    => decoder.decode(schema = schema.value, value).leftMap(index.toInt /: _)
    case Union.Modify(self, f, _) => decode(schema = self, value).map(f)
