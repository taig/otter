package io.taig.otter.codec

import io.taig.otter.Dictionary

import scala.annotation.tailrec

final class DictionaryEncoder[S[_], T](encoder: Encoder[S, T])
    extends Encoder[Dictionary.Write[S, *], List[(String, T)]]:
  @tailrec
  override def encode[A](schema: Dictionary.Write[S, A], a: A): List[(String, T)] = schema match
    case Dictionary.Linked(schema, _) =>
      a.map((key, value) => (key, encoder.encode(schema.value, value)))
    case Dictionary.Modify(self, _, f) => encode(schema = self, f(a))
    case Dictionary.Hashed(schema, _)  =>
      a.map((key, value) => (key, encoder.encode(schema.value, value))).toList
    case Dictionary.Write.Modify(self, f) => encode(schema = self, f(a))
