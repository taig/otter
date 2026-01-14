package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Optional
import io.taig.otter.Violations

final class OptionalDecoder[F[_], A](decoder: Decoder[F, A], empty: A => Boolean) extends Decoder[Optional[F, *], A]:
  override def decode[B](schema: Optional[F, B], value: A): Validated[Violations, B] = schema match
    case Optional.Modify(self, f, _)       => decode(schema = self, value).map(f)
    case Optional.Default(schema, default) =>
      if empty(value)
      then default.value.valid
      else decoder.decode(schema.value, value)
    case Optional.Root(schema) =>
      if empty(value)
      then None.valid
      else decoder.decode(schema.value, value).map(_.some)
