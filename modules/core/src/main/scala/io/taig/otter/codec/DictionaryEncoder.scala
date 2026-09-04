package io.taig.otter.codec

import cats.Monoid
import io.taig.otter.Dictionary

/** Writes a dictionary as what its members contribute, combined in arrival order.
  *
  * `member` is how a key and a value become that contribution and `M` what the dictionary combines them into, on the
  * same reasoning as [[FieldEncoder]].
  */
final class DictionaryEncoder[K[-_, +_], F[-_, +_], T, M: Monoid](
    key: Encoder[K, String],
    encoder: Encoder[F, T],
    member: (String, T) => M
) extends Encoder[[w, r] =>> Dictionary[K, F, w, r], M]:
  override def encode[W](schema: Dictionary[K, F, W, Any], w: W): M = schema match
    case Dictionary.Hashed(keys, reference, _, _) =>
      Monoid[M].combineAll:
        w.iterator.map((k, value) => member(key.encode(keys.value, k), encoder.encode(reference.value, value)))
    case Dictionary.Linked(keys, reference, _) =>
      Monoid[M].combineAll:
        w.iterator.map((k, value) => member(key.encode(keys.value, k), encoder.encode(reference.value, value)))
    case Dictionary.Modify(self, _, g) => encode(self, g(w))

object DictionaryEncoder:
  /** The pair form, which is what a format building a keyed structure out of values asks for. */
  def apply[K[-_, +_], F[-_, +_], T](
      key: Encoder[K, String],
      encoder: Encoder[F, T]
  ): DictionaryEncoder[K, F, T, List[(String, T)]] =
    DictionaryEncoder(key, encoder, (name, value) => List(name -> value))

  def apply[K[-_, +_], F[-_, +_], T, M: Monoid](
      key: Encoder[K, String],
      encoder: Encoder[F, T],
      member: (String, T) => M
  ): DictionaryEncoder[K, F, T, M] = new DictionaryEncoder(key, encoder, member)
