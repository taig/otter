package io.taig.otter.codec

import io.taig.otter.Dictionary

final class DictionaryEncoder[F[-_, +_], T](encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Dictionary[F, w, r], List[(String, T)]]:
  override def encode[W](schema: Dictionary[F, W, Any], w: W): List[(String, T)] = schema match
    case Dictionary.Hashed(reference, _) =>
      w.view.map((key, value) => (key, encoder.encode(reference.value, value))).toList
    case Dictionary.Linked(reference, _) =>
      w.map((key, value) => (key, encoder.encode(reference.value, value)))
    case Dictionary.Modify(self, _, g) => encode(self, g(w))
