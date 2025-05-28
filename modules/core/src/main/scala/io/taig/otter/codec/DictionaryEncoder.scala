package io.taig.otter.codec

import io.taig.otter.Dictionary

import scala.annotation.tailrec

final class DictionaryEncoder[S[_], T[_], U](key: Encoder[S, String], value: Encoder[T, U])
    extends Encoder[Dictionary[S, T, *], List[(String, U)]]:
  override def encode[A](schema: Dictionary[S, T, A], a: A): List[(String, U)] =
    encode(schema = schema.value, a)

  @tailrec
  def encode[A](schema: Dictionary.Value[S, T, A], a: A): List[(String, U)] = schema match
    case Dictionary.Value.Root(key, schema, _, _) =>
      a.map((name, value) =>
        (this.key.encode(schema = key.value, name), this.value.encode(schema = schema.value, value))
      )
    case Dictionary.Value.Modify(self, f, g) => encode(schema = self, g(a))
