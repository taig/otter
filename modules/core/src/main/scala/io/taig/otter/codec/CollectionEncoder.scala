package io.taig.otter.codec

import cats.Monoid
import io.taig.otter.Collection

/** Writes a collection as what its elements contribute, combined in arrival order.
  *
  * `element` is what one element contributes and `M` what the collection combines them into, on the same reasoning as
  * [[RecordEncoder]].
  */
final class CollectionEncoder[F[-_, +_], T, M: Monoid](encoder: Encoder[F, T], element: T => M)
    extends Encoder[[w, r] =>> Collection[F, w, r], M]:
  override def encode[W](schema: Collection[F, W, Any], w: W): M = schema match
    case Collection.Chained(reference, _) =>
      Monoid[M].combineAll(w.iterator.map(value => element(encoder.encode(reference.value, value))))
    case Collection.Indexed(reference, _) =>
      Monoid[M].combineAll(w.iterator.map(value => element(encoder.encode(reference.value, value))))
    case Collection.Linked(reference, _) =>
      Monoid[M].combineAll(w.iterator.map(value => element(encoder.encode(reference.value, value))))
    case Collection.Modify(self, _, g) => encode(self, g(w))

object CollectionEncoder:
  /** The container form, which is what a format building a sequence out of values asks for. */
  def apply[F[-_, +_], T](encoder: Encoder[F, T]): CollectionEncoder[F, T, Vector[T]] =
    CollectionEncoder(encoder, Vector.apply(_))

  def apply[F[-_, +_], T, M: Monoid](encoder: Encoder[F, T], element: T => M): CollectionEncoder[F, T, M] =
    new CollectionEncoder(encoder, element)
