package io.taig.otter.http

import org.http4s.Headers as Http4sHeaders
import org.http4s.Header as Http4sHeader
import io.taig.otter.*
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import cats.data.Validated.Valid
import cats.data.Validated.Invalid

object Http4sHeadersDecoder:
  def apply[A](
      headers: Headers[A],
      values: List[Http4sHeader.Raw]
  ): Validated[Violations, (List[Http4sHeader.Raw], A)] = headers match
    case Headers.Empty              => (values, ()).valid
    case Headers.Modify(self, f, g) => apply(headers = self, values).map(_.map(f))
    case Headers.Optional(self) =>
      val names = headers.toChain.map(_.name)

      if (names.exists(name => values.exists(_.name === name)))
      then apply(headers = self, values).map(_.map(_.some))
      else (values, none).valid
    case Headers.Root(header) => Http4sHeaderDecoder(header, values)
    case Headers.Zip(left, right) =>
      apply(headers = left, values) match
        case Validated.Valid((values, a)) => apply(headers = right, values).map(_.tupleLeft(a))
        case Validated.Invalid(left) =>
          apply(headers = right, values) match
            case Validated.Valid(_)       => left.invalid
            case Validated.Invalid(right) => (left |+| right).invalid
