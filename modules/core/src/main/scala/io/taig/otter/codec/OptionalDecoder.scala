package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Optional
import io.taig.otter.Violations

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class OptionalDecoder[F[-_, +_], T](decoder: Decoder[F, T], empty: T => Boolean)
    extends Decoder[[w, r] =>> Optional[F, w, r], T]:
  override def decode[R](schema: Optional[F, Nothing, R], value: T): Validated[Violations, R] =
    (schema: @unchecked) match
      case schema: Optional.Default[F, ?, R] =>
        if empty(value) then schema.default.value.valid else decoder.decode(schema.reference.value, value)
      case schema: Optional.Modify[F, ?, ?, ?, R] => decode(schema.self, value).map(schema.f)
      case schema: Optional.Root[F, ?, ?]         =>
        if empty(value) then none.asInstanceOf[R].valid
        else decoder.decode(schema.reference.value, value).map(_.some.asInstanceOf[R])
