package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Record[S, *], Chain[(String, T)]]:
  override def encode[A](schema: Record[S, A], a: A): Chain[(String, T)] = schema match
    case Record.Default(self, _)   => encode(schema = self, a)
    case Record.Empty              => Chain.empty
    case Record.Modify(self, _, g) => encode(schema = self, g(a))
    case Record.Optional(self)     => a.fold(Chain.empty)(encode(schema = self, _))
    case Record.Root(field)        =>
      Chain.one(field.name, encoder.encode(schema = field.schema.value, a))
    case Record.Zip(left, right) => encode(schema = left, a._1) ++ encode(schema = right, a._2)

object RecordEncoder:
  def apply[S[_], A](encoder: Encoder[S, A]): Encoder[Record[S, *], Chain[(String, A)]] =
    new RecordEncoder(encoder)
