package io.taig.otter.codec

import io.taig.otter.Union

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class UnionEncoder[F[- _, + _], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Union[F, w, r], T]:
  override def encode[W](schema: Union[F, W, Any], w: W): T = (schema: @unchecked) match
    case schema: Union.Coproduct[F, w1, ?, w2, ?] =>
      w.asInstanceOf[Either[w1, w2]].fold(encode(schema.left, _), encode(schema.right, _))
    case schema: Union.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
    case schema: Union.Root[F, W, ?]          => encoder.encode(schema.branch.value, w)
