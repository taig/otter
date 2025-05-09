package io.taig.otter.http

import io.taig.otter.Violations
import cats.data.Validated
import cats.syntax.all.*

object HeadersDataDecoder:
  object Remainders:
    def apply[A](headers: Headers[A], data: Headers.Data): Validated[Violations, (Headers.Data, A)] = headers match
      case Headers.Empty              => (data, ()).valid
      case Headers.Modify(self, f, _) => apply(headers = self, data).map(_.map(f))
      case Headers.Optional(self) =>
        val names = headers.toChain.map(_.name)
        if (names.exists(name => data.exists((key, _) => key === name)))
        then apply(headers = self, data).map(_.map(_.some))
        else (data, none).valid
      case Headers.Root(header) => HeaderDataDecoder.Remainders(header, data)
      case Headers.Zip(left, right) =>
        apply(headers = left, data) match
          case Validated.Valid((values, a)) => apply(headers = right, data).map(_.tupleLeft(a))
          case Validated.Invalid(violations) =>
            apply(headers = right, data).fold(violations |+| _, _ => violations).invalid
