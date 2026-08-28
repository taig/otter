package io.taig.otter.codec

import io.taig.otter.Constant

final class ConstantEncoder[F[- _, + _], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Constant[F, w, r], T]:
  override def encode[W](schema: Constant[F, W, Any], w: W): T = (schema: @unchecked) match
    case schema: Constant.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
    case schema: Constant.Root[F, ?]             => encoder.encode(schema.reference.value, schema.value.value)
