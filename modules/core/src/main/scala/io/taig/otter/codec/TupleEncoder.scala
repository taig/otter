package io.taig.otter.codec

import io.taig.otter.Tuple

final class TupleEncoder[F[_], T](encoder: Encoder[F, T], empty: T) extends Encoder[Tuple.Write[F, *], Vector[T]]:
  override def encode[A](schema: Tuple.Write[F, A], a: A): Vector[T] = schema match
    case Tuple.Default(self, _)      => encode(schema = self, a)
    case Tuple.Empty                 => Vector.empty
    case Tuple.Modify(self, _, f)    => encode(schema = self, f(a))
    case Tuple.Write.Modify(self, f) => encode(schema = self, f(a))
    // case Tuple.Optional(self)             => a.fold(Vector.fill(self.size.toInt)(empty))(encode(schema = self, _))
    case Tuple.Root(schema)               => Vector(encoder.encode(schema.value, a))
    case Tuple.Product(left, right)       => encode(schema = left, a._1) ++ encode(schema = right, a._2)
    case Tuple.Write.Optional(self)       => a.fold(Vector.fill(self.size.toInt)(empty))(encode(schema = self, _))
    case Tuple.Write.Product(left, right) => encode(schema = left, a._1) ++ encode(schema = right, a._2)
