package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.http.Header
import io.taig.otter.http.Headers

object HeaderDataDecoder extends Decoder.Remaining[Header, Headers.Data]:
  override def decodeRemaining[A](schema: Header[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    decodeRemaining(schema = schema.value, value)

  def decodeRemaining[A](schema: Header.Value[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    schema match
      case Header.Value.Root(name, schema) =>
        val (remainders, result) = value.collectFirstWithRemainders { case (`name`, value) => value }

        result
          .toValid(Violation.fromConstraint(constraint = Constraint.Generic.Required, actual = Data.Null))
          .leftMap(Violations.rootNec)
          .andThen(HeaderSchemaParser.decode(schema = schema.value, _).tupleLeft(remainders))
          .leftMap(s"$name" /: _)
      case Header.Value.Optional(self) =>
        val reference = schema.name
        if value.exists((name, _) => name === reference)
        then decodeRemaining(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Header.Value.Modify(self, f, _) => decodeRemaining(schema = self, value).map(_.map(f))
