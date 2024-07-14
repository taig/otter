package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json

object DictionaryJsonEncoder:
  def apply[A](schema: Dictionary.Via[Json, A], a: A): Option[List[(String, Json)]] = schema match
    case Dictionary.Optional(self) => a.flatMap(DictionaryJsonEncoder(self, _))
    case Dictionary.Root(_, key, value) =>
      a.map { case (a, b) => (ValueRequiredStringEncoder(key, a), JsonEncoder(value, b)) }.some
    case Dictionary.Transform(self, _, f) => DictionaryJsonEncoder(self, f(a))
