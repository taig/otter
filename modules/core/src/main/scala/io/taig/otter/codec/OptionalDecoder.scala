package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Optional
import io.taig.otter.Violations

final class OptionalDecoder[F[-_, +_], T](decoder: Decoder[F, T], empty: T => Boolean)
    extends Decoder[[w, r] =>> Optional[F, w, r], T]:
  override def decode[R](schema: Optional[F, Nothing, R], value: T): Validated[Violations, R] =
    schema match
      case Optional.Default(reference, default) =>
        if empty(value) then default.value.valid else decoder.decode(reference.value, value)
      case Optional.Modify(self, f, _) => decode(self, value).map(f)
      case Optional.Root(reference)    =>
        if empty(value) then Validated.Valid(None) else decoder.decode(reference.value, value).map(Some.apply)
