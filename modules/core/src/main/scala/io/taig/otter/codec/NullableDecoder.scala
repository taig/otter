package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Nullable
import io.taig.otter.Violations

final class NullableDecoder[S[_], T](decoder: Decoder[S, T], empty: T => Boolean) extends Decoder[Nullable[S, *], T]:
  override def decode[A](schema: Nullable[S, A], value: T): Validated[Violations, A] = schema match
    case Nullable.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Nullable.Default(reference, default) =>
      if empty(value)
      then default.valid
      else decoder.decode(schema = reference.value, value)
    case Nullable.Root(reference) =>
      if empty(value)
      then None.valid
      else decoder.decode(schema = reference.value, value).map(_.some)
    case Nullable.Void => ().valid

object NullableDecoder:
  final class Remaining[S[_], T](decoder: Decoder.Remaining[S, T], empty: T => Boolean)
      extends Decoder.Remaining[Nullable[S, *], T]:
    override def decodeRemaining[A](schema: Nullable[S, A], value: T): Validated[Violations, (T, A)] = schema match
      case Nullable.Modify(self, f, _) => decodeRemaining(schema = self, value).map(_.map(f))
      case Nullable.Default(reference, default) =>
        if empty(value)
        then (value, default).valid
        else decoder.decodeRemaining(schema = reference.value, value)
      case Nullable.Root(reference) =>
        if empty(value)
        then (value, none).valid
        else decoder.decodeRemaining(schema = reference.value, value).map(_.map(_.some))
      case Nullable.Void => (value, ()).valid
