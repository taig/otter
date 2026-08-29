package io.taig.otter.codec

import io.taig.otter.Tuple

final class TupleEncoder[F[-_, +_], T](encoder: Encoder[F, T], empty: T)
    extends Encoder[[w, r] =>> Tuple[F, w, r], Vector[T]]:
  override def encode[W](schema: Tuple[F, W, Any], w: W): Vector[T] = schema match
    case Tuple.Empty              => Vector.empty
    case Tuple.Default(self, _)   => encode(self, w)
    case Tuple.Modify(self, _, g) => encode(self, g(w))
    case Tuple.Optional(self)     =>
      w.fold(Vector.fill(self.schemas.length.toInt)(empty))(encode(self, _))
    case Tuple.Product(left, right) => encode(left, w._1) ++ encode(right, w._2)
    case Tuple.Root(schema)         => Vector(encoder.encode(schema.value, w))
