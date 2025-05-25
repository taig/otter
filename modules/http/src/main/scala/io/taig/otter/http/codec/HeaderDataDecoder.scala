package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.http.Header
import io.taig.otter.http.Headers
import io.taig.otter.http.Headers.Data

object HeaderDataDecoder extends Decoder.Remainding[Header, Headers.Data]:
  override def decodeRemainding[A](schema: Header[A], value: Data): Validated[Violations, (Data, A)] =
    schema match
      case Header.Root(name, schema) =>
        val (remainders, result) = value.collectFirstWithRemainders { case (`name`, value) => value }

        result
          .toValid(Violations.rootNec(Violation.required))
          .andThen: value =>
            HeaderValueParser.decode(schema = schema.value, value).tupleLeft(remainders)
          .leftMap(s"$name" /: _)
      case Header.Optional(self) =>
        val reference = schema.name
        if value.exists((name, _) => name === reference)
        then decodeRemainding(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Header.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
