package io.taig.otter.codec

import io.taig.otter.Dictionary

import scala.annotation.tailrec

final class DictionaryEncoder[F[_], T](encoder: Encoder[F, T])
    extends Encoder[Dictionary.Write[F, *], List[(String, T)]]:
  @tailrec
  override def encode[A](schema: Dictionary.Write[F, A], a: A): List[(String, T)] = schema match
    case Dictionary.Linked(schema, _) =>
      a.map((key, value) => (key, encoder.encode(schema.value, value)))
    case Dictionary.Modify(self, _, f) => encode(schema = self, f(a))
    case Dictionary.Hashed(schema, _)  =>
      a.map((key, value) => (key, encoder.encode(schema.value, value))).toList
    case Dictionary.Write.Modify(self, f) => encode(schema = self, f(a))
