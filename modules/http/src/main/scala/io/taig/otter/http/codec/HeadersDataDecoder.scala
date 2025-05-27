package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Headers

object HeadersDataDecoder extends Decoder.Remaining[Headers, Headers.Data]:
  override def decodeRemaining[A](schema: Headers[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    decodeRemaining(schema = schema.self, value)

  def decodeRemaining[A](schema: Headers.Value[A], value: Headers.Data): Validated[Violations, (Headers.Data, A)] =
    schema match
      case Headers.Value.Empty              => (value, ()).valid
      case Headers.Value.Modify(self, f, _) => decodeRemaining(schema = self, value).map(_.map(f))
      case Headers.Value.Optional(self) =>
        val names = schema.toChain.map(_.name)
        if (names.exists(name => value.exists((key, _) => key === name)))
        then decodeRemaining(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Headers.Value.Root(header) => HeaderDataDecoder.decodeRemaining(header, value)
      case Headers.Value.Zip(left, right) =>
        decodeRemaining(schema = left, value) match
          case Validated.Valid((values, a)) => decodeRemaining(schema = right, value).map(_.tupleLeft(a))
          case Validated.Invalid(violations) =>
            decodeRemaining(schema = right, value).fold(violations |+| _, _ => violations).invalid
