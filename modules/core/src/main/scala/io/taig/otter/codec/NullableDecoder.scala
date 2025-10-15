package io.taig.otter.codec

import cats.data.Validated

import io.taig.otter.Violations
import io.taig.otter.Nullable
import cats.syntax.all.*

final class NullableDecoder[-S[_], T](decoder: Decoder[S, T], empty: T => Boolean) extends Decoder[Nullable[S, *], T]:
  override def decode[A](schema: Nullable[S, A], value: T): Validated[Violations, A] = schema match
    case Nullable.Modify(self, f, _)       => decode(schema = self, value).map(f)
    case Nullable.Default(schema, default) =>
      if empty(value)
      then default.value.valid
      else decoder.decode(schema = schema.value, value)
    case Nullable.Optional(schema) =>
      if empty(value)
      then None.valid
      else decoder.decode(schema = schema.value, value).map(_.some)

object NullableDecoder:
  def apply[S[_], T](decoder: Decoder[S, T], empty: T => Boolean): Decoder[Nullable[S, *], T] =
    new NullableDecoder(decoder, empty)
