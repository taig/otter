package io.taig.otter.codec

import io.taig.otter.Tuple

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class TupleEncoder[F[- _, + _], T](encoder: Encoder[F, T], empty: T)
    extends Encoder[[w, r] =>> Tuple[F, w, r], Vector[T]]:
  override def encode[W](schema: Tuple[F, W, Any], w: W): Vector[T] = (schema: @unchecked) match
    case Tuple.Empty                            => Vector.empty
    case schema: Tuple.Default[F, W, ?]         => encode(schema.self, w)
    case schema: Tuple.Modify[F, ?, ?, W, ?]   => encode(schema.self, schema.g(w))
    case schema: Tuple.Optional[F, w0, ?]       =>
      w.asInstanceOf[Option[w0]].fold(Vector.fill(schema.self.schemas.length.toInt)(empty))(encode(schema.self, _))
    case schema: Tuple.Product[F, w1, ?, w2, ?] =>
      val (left, right) = w.asInstanceOf[(w1, w2)]
      encode(schema.left, left) ++ encode(schema.right, right)
    case schema: Tuple.Root[F, W, ?] => Vector(encoder.encode(schema.schema.value, w))
