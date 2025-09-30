package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Dictionary
import io.taig.otter.Violations

// TODO validation
final class DictionaryDecoder[S[_], T[_], U](key: Decoder[S, String], value: Decoder[T, U])
    extends Decoder[Dictionary[S, T, *], List[(String, U)]]:
  override def decode[A](schema: Dictionary[S, T, A], values: List[(String, U)]): Validated[Violations, A] =
    decode(schema = schema.value, values)

  def decode[A](schema: Dictionary.Value[S, T, A], values: List[(String, U)]): Validated[Violations, A] = schema match
    case Dictionary.Value.Root(key, schema, _) =>
      values.traverse: (name, value) =>
        (
          this.key.decode(schema = key.value, name),
          this.value.decode(schema = schema.value, value)
        ).tupled.leftMap(name /: _)
    case Dictionary.Value.Modify(self, f, _) => decode(schema = self, values).map(f)
