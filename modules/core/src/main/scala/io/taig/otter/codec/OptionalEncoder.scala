package io.taig.otter.codec

import io.taig.otter.Optional

import scala.annotation.tailrec

final class OptionalEncoder[F[_], T](encoder: Encoder[F, T], empty: T) extends Encoder[Optional.Write[F, *], T]:
  @tailrec
  override def encode[A](schema: Optional.Write[F, A], a: A): T = schema match
    case Optional.Default(schema, _)    => encoder.encode(schema.value, a)
    case Optional.Modify(self, _, f)    => encode(schema = self, f(a))
    case Optional.Root(schema)          => a.fold(empty)(encoder.encode(schema.value, _))
    case Optional.Write.Modify(self, f) => encode(schema = self, f(a))
