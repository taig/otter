package io.taig.otter

import scala.annotation.tailrec

final class DictionaryEncoder[S[_], T[_], U](key: Encoder[S, String], value: Encoder[T, U])
    extends Encoder[Dictionary[S, T, *], List[(String, U)]]:
  @tailrec
  def apply[A](schema: Dictionary[S, T, A], a: A): List[(String, U)] = schema match
    case Dictionary.Root(key, schema, _, _, _) =>
      a.map((name, value) => (this.key(schema = key.value, name), this.value(schema = schema.value, value)))
    case Dictionary.Modify(self, f, g) => apply(schema = self, g(a))
