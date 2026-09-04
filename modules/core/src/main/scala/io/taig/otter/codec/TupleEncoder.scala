package io.taig.otter.codec

import cats.Monoid
import cats.syntax.all.*
import io.taig.otter.Tuple

/** Writes a tuple as what its members contribute, combined in position order.
  *
  * `element` is what one member contributes and `M` what the tuple combines them into, on the same reasoning as
  * [[RecordEncoder]]. `empty` is what a member of an absent optional tuple writes, and it is written once per schema
  * that tuple holds, so that the positions after it still line up.
  */
final class TupleEncoder[F[-_, +_], T, M: Monoid](encoder: Encoder[F, T], empty: T, element: T => M)
    extends Encoder[[w, r] =>> Tuple[F, w, r], M]:
  override def encode[W](schema: Tuple[F, W, Any], w: W): M = schema match
    case Tuple.Empty              => Monoid[M].empty
    case Tuple.Default(self, _)   => encode(self, w)
    case Tuple.Modify(self, _, g) => encode(self, g(w))
    case Tuple.Optional(self)     =>
      w.fold(Monoid[M].combineN(element(empty), self.schemas.length.toInt))(encode(self, _))
    case Tuple.Product(left, right) => encode(left, w._1) |+| encode(right, w._2)
    case Tuple.Root(schema)         => element(encoder.encode(schema.value, w))

object TupleEncoder:
  /** The container form, which is what a format building a sequence out of values asks for. */
  def apply[F[-_, +_], T](encoder: Encoder[F, T], empty: T): TupleEncoder[F, T, Vector[T]] =
    TupleEncoder(encoder, empty, Vector.apply(_))

  def apply[F[-_, +_], T, M: Monoid](
      encoder: Encoder[F, T],
      empty: T,
      element: T => M
  ): TupleEncoder[F, T, M] = new TupleEncoder(encoder, empty, element)
