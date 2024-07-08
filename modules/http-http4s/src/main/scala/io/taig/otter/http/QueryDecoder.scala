package io.taig.otter.http

import io.taig.otter.http as Base
import io.taig.otter.http.Plain.*
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.ValueStringDecoder

object QueryDecoder:
  def apply[A](
      query: Query[A],
      values: List[(String, Option[String])]
  ): Decoder.Result[Option[String], (List[(String, Option[String])], A)] = query match
    case Base.Query.Transform(self, f) => transform(self, f, values)
    case Base.Query.Root(name, schema) =>
      val (value, remainders) = values.findWithRemainders { case (`name`, value) => value }
      ValueStringDecoder(schema, value.flatten).tupleLeft(remainders)

  def transform[A, B](
      self: Query[A],
      f: A => B,
      values: List[(String, Option[String])]
  ): Decoder.Result[Option[String], (List[(String, Option[String])], B)] =
    QueryDecoder(self, values).map(_.map(f))
