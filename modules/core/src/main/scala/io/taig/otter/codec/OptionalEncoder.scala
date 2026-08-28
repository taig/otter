package io.taig.otter.codec

import io.taig.otter.Optional

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class OptionalEncoder[F[-_, +_], T](encoder: Encoder[F, T], empty: T)
    extends Encoder[[w, r] =>> Optional[F, w, r], T]:
  override def encode[W](schema: Optional[F, W, Any], w: W): T = (schema: @unchecked) match
    case schema: Optional.Default[F, W, ?]      => encoder.encode(schema.reference.value, w)
    case schema: Optional.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
    case schema: Optional.Root[F, w0, ?]        =>
      w.asInstanceOf[Option[w0]].fold(empty)(encoder.encode(schema.reference.value, _))
