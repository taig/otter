package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class RecordEncoder[F[- _, + _], T](encoder: Encoder[F, Chain[(String, T)]])
    extends Encoder[[w, r] =>> Record[F, w, r], Chain[(String, T)]]:
  override def encode[W](schema: Record[F, W, Any], w: W): Chain[(String, T)] = (schema: @unchecked) match
    case Record.Empty                          => Chain.empty
    case schema: Record.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
    case schema: Record.Product[F, w1, ?, w2, ?] =>
      val (left, right) = w.asInstanceOf[(w1, w2)]
      encode(schema.left, left) ++ encode(schema.right, right)
    case schema: Record.Root[F, W, ?] => encoder.encode(schema.field.value, w)
