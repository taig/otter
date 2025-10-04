package io.taig.otter.codec

import io.taig.otter.Dictionary
import io.taig.otter.Dictionary.Modify
import io.taig.otter.Dictionary.Root

import scala.annotation.tailrec

final class DictionaryEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Dictionary[S, *], List[(String, T)]]:
  @tailrec
  override def encode[A](schema: Dictionary[S, A], a: A): List[(String, T)] = schema match
    case Modify(self, _, g) => encode(schema = self, g(a))
    case Root(schema, _)    => a.map((key, value) => (key, encoder.encode(schema = schema.value, value)))

object DictionaryEncoder:
  def apply[S[_], A](encoder: Encoder[S, A]): Encoder[Dictionary[S, *], List[(String, A)]] = new DictionaryEncoder(
    encoder
  )
