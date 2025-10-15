package io.taig.otter.codec

import io.taig.otter.Dictionary
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations

final class DictionaryDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder[Dictionary[S, *], List[(String, T)]]:
  override def decode[A](schema: Dictionary[S, A], values: List[(String, T)]): Validated[Violations, A] = schema match
    case Dictionary.Modify(self, f, _)       => decode(schema = self, values).map(f)
    case Dictionary.Root(schema, validation) =>
      validation
        .validate(values)
        .toValidated
        .leftMap(Violations.apply)
        .andThen: _ =>
          values.traverse((key, value) => decoder.decode(schema = schema.value, value).tupleLeft(key).leftMap(key /: _))

object DictionaryDecoder:
  def apply[S[_], T](decoder: Decoder[S, T]): Decoder[Dictionary[S, *], List[(String, T)]] =
    new DictionaryDecoder(decoder)
