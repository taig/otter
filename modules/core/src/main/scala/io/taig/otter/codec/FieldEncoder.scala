package io.taig.otter.codec

import cats.Monoid
import cats.data.Chain
import io.taig.otter.Field

/** Writes a field as what it contributes to a record.
  *
  * `absent` is what an absent optional field contributes: `None` drops the key, `Some(value)` keeps it and writes
  * `value`. A field that holds a default is never absent, so it always writes.
  *
  * `member` is how a name and a value become that contribution, and `M` what a record combines them into.
  * [[FieldEncoder.apply]] is the pair form, which is what a format building a keyed structure asks for; a format
  * writing into an output hands over a write that writes the key and then the value, and no pair is built.
  */
final class FieldEncoder[F[-_, +_], T, M: Monoid](encoder: Encoder[F, T], absent: Option[T], member: (String, T) => M)
    extends Encoder[[w, r] =>> Field[F, w, r], M]:
  override def encode[W](field: Field[F, W, Any], w: W): M = field match
    case Field.Default(self, _)   => encode(self, w)
    case Field.Modify(self, _, g) => encode(self, g(w))
    case Field.Optional(self)     =>
      w.fold(absent.fold(Monoid[M].empty)(member(self.name, _)))(encode(self, _))
    case Field.Root(name, reference) => member(name, encoder.encode(reference.value, w))

object FieldEncoder:
  /** The pair form, which is what a format building a keyed structure out of values asks for. */
  def apply[F[-_, +_], T](encoder: Encoder[F, T], absent: Option[T]): FieldEncoder[F, T, Chain[(String, T)]] =
    FieldEncoder(encoder, absent, (name, value) => Chain.one(name -> value))

  def apply[F[-_, +_], T, M: Monoid](
      encoder: Encoder[F, T],
      absent: Option[T],
      member: (String, T) => M
  ): FieldEncoder[F, T, M] = new FieldEncoder(encoder, absent, member)
