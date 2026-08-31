package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Field

/** Writes a field as the pairs it contributes to a record.
  *
  * `absent` is what an absent optional field contributes: `None` drops the key, `Some(value)` keeps it and writes
  * `value`. A field that holds a default is never absent, so it always writes.
  */
final class FieldEncoder[F[-_, +_], T](encoder: Encoder[F, T], absent: Option[T])
    extends Encoder[[w, r] =>> Field[F, w, r], Chain[(String, T)]]:
  override def encode[W](field: Field[F, W, Any], w: W): Chain[(String, T)] = field match
    case Field.Default(self, _)      => encode(self, w)
    case Field.Modify(self, _, g)    => encode(self, g(w))
    case Field.Optional(self)        => w.fold(Chain.fromOption(absent.map(self.name -> _)))(encode(self, _))
    case Field.Root(name, reference) => Chain.one(name -> encoder.encode(reference.value, w))
