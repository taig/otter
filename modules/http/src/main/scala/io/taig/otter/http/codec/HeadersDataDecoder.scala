package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Headers

object HeadersDataDecoder extends Decoder.Remainding[Headers, Headers.Data]:
  override def decodeRemainding[A](schema: Headers[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    decodeRemainding(schema = schema.self, value)

  def decodeRemainding[A](schema: Headers.Value[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    schema match
      case Headers.Value.Empty              => (value, ()).valid
      case Headers.Value.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
      case Headers.Value.Optional(self) =>
        val names = schema.toChain.map(_.name)
        if (names.exists(name => value.exists((key, _) => key === name)))
        then decodeRemainding(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Headers.Value.Root(header) => HeaderDataDecoder.decodeRemainding(header, value)
      case Headers.Value.Zip(left, right) =>
        decodeRemainding(schema = left, value) match
          case Validated.Valid((values, a)) => decodeRemainding(schema = right, value).map(_.tupleLeft(a))
          case Validated.Invalid(violations) =>
            decodeRemainding(schema = right, value).fold(violations |+| _, _ => violations).invalid
