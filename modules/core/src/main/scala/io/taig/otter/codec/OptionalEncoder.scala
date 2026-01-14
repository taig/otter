package io.taig.otter.codec

import io.taig.otter.Optional

import scala.annotation.tailrec

final class OptionalEncoder[F[_], A](encoder: Encoder[F, A], empty: A) extends Encoder[Optional.Write[F, *], A]:
  @tailrec
  override def encode[B](schema: Optional.Write[F, B], a: B): A = schema match
    case Optional.Default(schema, _)    => encoder.encode(schema.value, a)
    case Optional.Modify(self, _, f)    => encode(schema = self, f(a))
    case Optional.Root(schema)          => a.fold(empty)(encoder.encode(schema.value, _))
    case Optional.Write.Modify(self, f) => encode(schema = self, f(a))
