package io.taig.otter.codec

import io.taig.otter.Collection

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class CollectionEncoder[F[-_, +_], T](encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Collection[F, w, r], Seq[T]]:
  override def encode[W](schema: Collection[F, W, Any], w: W): Seq[T] = (schema: @unchecked) match
    case schema: Collection.Chained[F, w0, ?] =>
      w.asInstanceOf[cats.data.Chain[w0]].map(encoder.encode(schema.reference.value, _)).toList
    case schema: Collection.Indexed[F, w0, ?] =>
      w.asInstanceOf[Vector[w0]].map(encoder.encode(schema.reference.value, _))
    case schema: Collection.Linked[F, w0, ?] =>
      w.asInstanceOf[List[w0]].map(encoder.encode(schema.reference.value, _))
    case schema: Collection.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
