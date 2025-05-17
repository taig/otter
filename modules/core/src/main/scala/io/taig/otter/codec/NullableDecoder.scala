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

object NullableDecoder:
  final class Remainding[S[_], T](decoder: Decoder.Remainding[S, T], empty: T => Boolean)
      extends Decoder.Remainding[Nullable[S, *], T]:
    override def decodeRemainding[A](schema: Nullable[S, A], value: T): Validated[Violations, (T, A)] = schema match
      case Nullable.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
      case Nullable.Default(reference, default, _) =>
        if empty(value)
        then (value, default).valid
        else decoder.decodeRemainding(schema = reference.value, value)
      case Nullable.Root(reference, _) =>
        if empty(value)
        then (value, none).valid
        else decoder.decodeRemainding(schema = reference.value, value).map(_.map(_.some))
      case Nullable.Void(_) => (value, ()).valid
