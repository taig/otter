package io.taig.otter.codec

import cats.Monoid
import cats.data.Chain
import io.taig.otter.Field

/** Writes a field as what it contributes to a record.
  *
  * `empty` is the format's explicit empty representation. The field contract decides whether to emit it or omit the
  * member. A defaulted field delegates to its payload encoder.
  *
  * `member` is how a name and a value become that contribution, and `M` what a record combines them into.
  * [[FieldEncoder.apply]] is the pair form, which is what a format building a keyed structure asks for; a format
  * writing into an output hands over a write that writes the key and then the value, and no pair is built.
  */
final class FieldEncoder[F[-_, +_], T, M: Monoid](encoder: Encoder[F, T], empty: T, member: (String, T) => M)
    extends Encoder[Field[F, *, *], M]:
  override def encode[W](field: Field[F, W, Any], w: W): M = field match
    case Field.Default(self, _, _)      => encode(self, w)
    case Field.Modify(self, _, g)       => encode(self, g(w))
    case Field.Optional(self, presence) =>
      w.fold(if presence.writesEmpty then member(self.name, empty) else Monoid[M].empty)(encode(self, _))
    case Field.Root(name, reference) => member(name, encoder.encode(reference.value, w))

object FieldEncoder:
  /** The pair form, which is what a format building a keyed structure out of values asks for. */
  def apply[F[-_, +_], T](encoder: Encoder[F, T], empty: T): FieldEncoder[F, T, Chain[(String, T)]] =
    FieldEncoder(encoder, empty, (name, value) => Chain.one(name -> value))

  def apply[F[-_, +_], T, M: Monoid](
      encoder: Encoder[F, T],
      empty: T,
      member: (String, T) => M
  ): FieldEncoder[F, T, M] = new FieldEncoder(encoder, empty, member)
