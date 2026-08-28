package io.taig.otter.codec

import io.taig.otter.Dictionary

import scala.collection.immutable.SortedMap

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class DictionaryEncoder[F[-_, +_], T](encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Dictionary[F, w, r], List[(String, T)]]:
  override def encode[W](schema: Dictionary[F, W, Any], w: W): List[(String, T)] = (schema: @unchecked) match
    case schema: Dictionary.Hashed[F, w0, ?] =>
      w.asInstanceOf[SortedMap[String, w0]]
        .view
        .map((key, value) => (key, encoder.encode(schema.reference.value, value)))
        .toList
    case schema: Dictionary.Linked[F, w0, ?] =>
      w.asInstanceOf[List[(String, w0)]].map((key, value) => (key, encoder.encode(schema.reference.value, value)))
    case schema: Dictionary.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
