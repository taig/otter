package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

final class DictionaryDecoder[S[_], T[_], U](key: Decoder[S, String], value: Decoder[T, U])
    extends Decoder[Dictionary[S, T, *], List[(String, U)]]:
  def apply[A](schema: Dictionary[S, T, A], values: List[(String, U)]): Validated[Violations, A] = schema match
    case Dictionary.Root(key, schema, _, _, _) =>
      values.traverse: (name, value) =>
        (
          this.key(schema = key.value, name),
          this.value(schema = schema.value, value)
        ).tupled.leftMap(name /: _)
    case Dictionary.Modify(self, f, _) => apply(schema = self, values).map(f)
