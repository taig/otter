package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Optional
import io.taig.otter.Violations

final class OptionalDecoder[F[_], T](decoder: Decoder[F, T], empty: T => Boolean)
    extends Decoder[Optional.Read[F, *], T]:
  override def decode[A](schema: Optional.Read[F, A], value: T): Validated[Violations, A] = schema match
    case Optional.Modify(self, f, _)       => decode(schema = self, value).map(f)
    case Optional.Read.Modify(self, f)     => decode(schema = self, value).map(f)
    case Optional.Default(schema, default) =>
      if empty(value)
      then default.value.valid
      else decoder.decode(schema.value, value)
    case Optional.Root(schema) =>
      if empty(value)
      then None.valid
      else decoder.decode(schema.value, value).map(_.some)
