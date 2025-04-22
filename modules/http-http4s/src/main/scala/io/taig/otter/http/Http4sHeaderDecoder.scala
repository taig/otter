package io.taig.otter.http

import org.http4s.Header as Http4sHeader
import io.taig.otter.*
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.otter.Violation

object Http4sHeaderDecoder:
  def apply[A](header: Header[A], values: List[Http4sHeader.Raw]): Validated[Violations, (List[Http4sHeader.Raw], A)] =
    header match
      case Header.Root(name, codec, metadata) =>
        val (remainders, value) = values.collectFirstWithRemainders { case Http4sHeader.Raw(`name`, value) => value }

        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen: value =>
            val explode = metadata.get(HttpKeys.explode).getOrElse(false)
            HttpHeaderParser(explode)(codec = codec.value, value).tupleLeft(remainders)
      case Header.Optional(self) =>
        if values.exists(_.name === header.name)
        then apply(header = self, values).map(_.map(_.some))
        else (values, none).valid
      case Header.Modify(self, f, _) => apply(header = self, values).map(_.map(f))
