package io.taig.otter.codec

import io.taig.otter.Tuple

final class TupleEncoder[-S[_], T](encoder: Encoder[S, T], empty: T) extends Encoder[Tuple[S, *], Vector[T]]:
  override def encode[A](schema: Tuple[S, A], a: A): Vector[T] = schema match
    case Tuple.Default(self, _)   => encode(schema = self, a)
    case Tuple.Empty              => Vector.empty
    case Tuple.Modify(self, _, g) => encode(schema = self, g(a))
    case Tuple.Optional(self)     => a.fold(Vector.fill(self.size)(empty))(encode(schema = self, _))
    case Tuple.Root(schema)       => Vector(encoder.encode(schema = schema.value, a))
    case Tuple.Zip(left, right)   => encode(schema = left, a._1) ++ encode(schema = right, a._2)

object TupleEncoder:
  def apply[S[_], T](encoder: Encoder[S, T], empty: T): TupleEncoder[S, T] =
    new TupleEncoder(encoder, empty)
