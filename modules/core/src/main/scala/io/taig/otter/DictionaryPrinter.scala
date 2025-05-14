package io.taig.otter

import scala.annotation.tailrec
import io.taig.otter.schema.Dictionary

final class DictionaryPrinter[S[_], T[_], U, V](key: Encoder[S, U], value: Encoder[T, V])
    extends Encoder[Dictionary[S, T, *], List[(U, V)]]:
  @tailrec
  def apply[A](schema: Dictionary[S, T, A], a: A): List[(U, V)] = schema match
    case Dictionary.Root(key, schema, _, _, _) =>
      a.map((name, value) => (this.key(schema = key.value, name), this.value(schema = schema.value, value)))
    case Dictionary.Modify(self, f, g) => apply(schema = self, g(a))
