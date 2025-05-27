package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.http.Header
import io.taig.otter.http.Headers

object HeaderDataDecoder extends Decoder.Remainding[Header, Headers.Data]:
  override def decodeRemainding[A](schema: Header[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    decodeRemainding(schema = schema.self, value)

  def decodeRemainding[A](schema: Header.Value[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    schema match
      case Header.Value.Root(name, schema) =>
        val (remainders, result) = value.collectFirstWithRemainders { case (`name`, value) => value }

        result
          .toValid(Violations.rootNec(Violation.required))
          .andThen: value =>
            HeaderSchemaParser.decode(schema = schema.value, value).tupleLeft(remainders)
          .leftMap(s"$name" /: _)
      case Header.Value.Optional(self) =>
        val reference = schema.name
        if value.exists((name, _) => name === reference)
        then decodeRemainding(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Header.Value.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
