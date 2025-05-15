package io.taig.otter.codec

import io.taig.otter.Tuple

final class TupleEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Tuple[S, *], Vector[T]]:
  def apply[A](schema: Tuple[S, A], a: A): Vector[T] = schema match
    case Tuple.Empty(_)            => Vector.empty
    case Tuple.Modify(self, _, g)  => apply(schema = self, g(a))
    case Tuple.Root(schema, _)      => Vector(encoder(schema = schema.value, a))
    case Tuple.Zip(left, right, _) => apply(schema = left, a._1) ++ apply(schema = right, a._2)
