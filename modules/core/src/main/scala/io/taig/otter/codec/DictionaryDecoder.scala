package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Dictionary
import io.taig.otter.Violations

final class DictionaryDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder[Dictionary[S, *], List[(String, T)]]:
  override def decode[A](schema: Dictionary[S, A], values: List[(String, T)]): Validated[Violations, A] = ???
  // schema match
  //   case Dictionary.Modify(self, f, _)       => decode(schema = self, values).map(f)
  //   case Dictionary.Root(schema, validation) =>
  //     values
  //       .traverse((key, value) => decoder.decode(schema = schema.value, value).tupleLeft(key).leftMap(key /: _))
  //       .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))

object DictionaryDecoder:
  def apply[S[_], T](decoder: Decoder[S, T]): Decoder[Dictionary[S, *], List[(String, T)]] =
    new DictionaryDecoder(decoder)
