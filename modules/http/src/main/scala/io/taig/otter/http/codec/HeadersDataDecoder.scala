package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Headers
import io.taig.otter.http.Headers.Data

object HeadersDataDecoder extends Decoder.Remainding[Headers, Headers.Data]:
  override def decodeRemainding[A](schema: Headers[A], value: Data): Validated[Violations, (Data, A)] =
    schema match
      case Headers.Empty              => (value, ()).valid
      case Headers.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
      case Headers.Optional(self) =>
        val names = schema.toChain.map(_.name)
        if (names.exists(name => value.exists((key, _) => key === name)))
        then decodeRemainding(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Headers.Root(header) => HeaderDataDecoder.decodeRemainding(header, value)
      case Headers.Zip(left, right) =>
        decodeRemainding(schema = left, value) match
          case Validated.Valid((values, a)) => decodeRemainding(schema = right, value).map(_.tupleLeft(a))
          case Validated.Invalid(violations) =>
            decodeRemainding(schema = right, value).fold(violations |+| _, _ => violations).invalid
