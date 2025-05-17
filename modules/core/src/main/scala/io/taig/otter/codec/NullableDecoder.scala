package io.taig.otter.codec

import io.taig.otter.Nullable
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*

final class NullableDecoder[S[_], T](decoder: Decoder[S, T], empty: T => Boolean) extends Decoder[Nullable[S, *], T]:
  override def decode[A](schema: Nullable[S, A], value: T): Validated[Violations, A] = schema match
    case Nullable.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Nullable.Default(reference, default, _) =>
      if empty(value)
      then default.valid
      else decoder.decode(schema = reference.value, value)
    case Nullable.Root(reference, _) =>
      if empty(value)
      then None.valid
      else decoder.decode(schema = reference.value, value).map(_.some)
    case Nullable.Void(_) => ().valid
