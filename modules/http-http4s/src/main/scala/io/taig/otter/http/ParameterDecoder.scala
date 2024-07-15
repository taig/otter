package io.taig.otter.http

import io.taig.otter.http.*
import io.taig.otter.*
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.ValueStringDecoder
import cats.data.Chain

object ParameterDecoder:
  def apply[A](
      query: Parameter[A],
      values: Chain[(String, Option[String])]
  ): Decoder.Result[Option[String], (Chain[(String, Option[String])], A)] = query match
    case Parameter.Transform(self, f, _) => ParameterDecoder(self, values).map(_.map(f))
    case Parameter.Root(_, name, schema) =>
      val (value, remainders) = values.findWithRemainders { case (`name`, value) => value }
      ValueStringDecoder(schema, value.flatten).tupleLeft(remainders)
