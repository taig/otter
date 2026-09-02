package io.taig.otter.codec

import io.taig.otter.Dictionary

final class DictionaryEncoder[K[-_, +_], F[-_, +_], T](key: Encoder[K, String], encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Dictionary[K, F, w, r], List[(String, T)]]:
  override def encode[W](schema: Dictionary[K, F, W, Any], w: W): List[(String, T)] = schema match
    case Dictionary.Hashed(keys, reference, _, _) =>
      w.view.map((k, value) => (key.encode(keys.value, k), encoder.encode(reference.value, value))).toList
    case Dictionary.Linked(keys, reference, _) =>
      w.map((k, value) => (key.encode(keys.value, k), encoder.encode(reference.value, value)))
    case Dictionary.Modify(self, _, g) => encode(self, g(w))
