package io.taig.otter.codec

import io.taig.otter.Tuple

final class TupleEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Tuple[S, *], Vector[T]]:
  override def encode[A](schema: Tuple[S, A], a: A): Vector[T] = encode(schema = schema.value, a)

  def encode[A](schema: Tuple.Value[S, A], a: A): Vector[T] = schema match
    case Tuple.Value.Empty              => Vector.empty
    case Tuple.Value.Modify(self, _, g) => encode(schema = self, g(a))
    case Tuple.Value.Root(schema)       => Vector(encoder.encode(schema = schema.value, a))
    case Tuple.Value.Zip(left, right)   => encode(schema = left, a._1) ++ encode(schema = right, a._2)
