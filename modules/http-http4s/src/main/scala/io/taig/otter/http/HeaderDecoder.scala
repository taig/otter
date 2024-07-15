package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import io.taig.otter.http.*
import io.taig.otter.*
import io.taig.otter.ValueStringDecoder
import cats.syntax.all.*
import cats.data.Chain

object HeaderDecoder:
  def apply[A](
      header: Header[A],
      values: Chain[Http4sHeader.Raw]
  ): Decoder.Result[Option[String], (Chain[Http4sHeader.Raw], A)] = header match
    case Header.Root(_, name, schema) =>
      val (value, remainders) = values.findWithRemainders { case Http4sHeader.Raw(`name`, value) => value }
      ValueStringDecoder(schema, value).tupleLeft(remainders)
    case Header.Transform(self, f, _) => HeaderDecoder(self, values).map(_.map(f))
