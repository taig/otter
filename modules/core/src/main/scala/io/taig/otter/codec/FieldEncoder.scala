package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Field

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class FieldEncoder[F[-_, +_], T](encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Field[F, w, r], Chain[(String, T)]]:
  override def encode[W](field: Field[F, W, Any], w: W): Chain[(String, T)] = (field: @unchecked) match
    case field: Field.Default[F, W, ?]      => encode(field.self, w)
    case field: Field.Modify[F, ?, ?, W, ?] => encode(field.self, field.g(w))
    case field: Field.Optional[F, w0, ?]    => w.asInstanceOf[Option[w0]].fold(Chain.empty)(encode(field.self, _))
    case field: Field.Root[F, W, ?]         => Chain.one(field.name -> encoder.encode(field.reference.value, w))
