package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import cats.syntax.all.*
import io.taig.otter.ValueRequiredStringEncoder
import io.circe.Json

object DictionaryJsonEncoder:
  def apply[A](schema: Dictionary.Writer.Via[Json, A], a: A): Option[List[(String, Json)]] = schema match
    case Base.Dictionary.Optional(self)            => optional(self, a)
    case Base.Dictionary.Root(key, value)          => root(key, value, a).some
    case Base.Dictionary.Transform(self, _, f)     => transform(self, f, a)
    case Base.Dictionary.Writer.Optional(self)     => optional(self, a)
    case Base.Dictionary.Writer.Root(key, value)   => root(key, value, a).some
    case Base.Dictionary.Writer.Transform(self, f) => transform(self, f, a)

  def optional[A](self: Dictionary.Writer.Via[Json, A], a: Option[A]): Option[List[(String, Json)]] =
    a.flatMap(DictionaryJsonEncoder(self, _))

  def root[A, B](
      key: Value.Required.Writer.Via[Json, A],
      value: Schema.Writer.Via[Json, B],
      abs: List[(A, B)]
  ): List[(String, Json)] =
    abs.map { case (a, b) => (ValueRequiredStringEncoder(key, a), JsonEncoder(value, b)) }

  def transform[A, B](self: Dictionary.Writer.Via[Json, A], f: B => A, b: B): Option[List[(String, Json)]] =
    DictionaryJsonEncoder(self, f(b))
