package io.taig.otter.codec

import io.taig.otter.Coerce

final class CoerceEncoder[F[-_, +_], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Coerce[F, w, r], T]:
  override def encode[W](schema: Coerce[F, W, Any], w: W): T = (schema: @unchecked) match
    case schema: Coerce.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
    case schema: Coerce.Root[F, W, ?]         => encoder.encode(schema.reference.value, w)
