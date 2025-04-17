package io.taig.otter.http

import org.http4s.Header.Raw as Http4sHeader
import io.taig.otter.Decoder
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.otter.http.HttpKeys.*
import io.taig.otter.Violation

object Http4sHeaderDecoder extends Decoder[Header, Option[Http4sHeader]]:
  override def apply[A](header: Header[A], value: Option[Http4sHeader]): Validated[Violations, A] = header match
    case Header.Root(name, codec, metadata) =>
      value
        .toValid(Violations.rootNec(Violation.required))
        .andThen: header =>
          HttpHeaderParser(explode = metadata.get(explode).getOrElse(false))(
            codec = codec.value,
            value = header.value
          )
    case Header.Optional(self) =>
      value match
        case Some(value) => apply(header = self, value = value.some).map(_.some)
        case None        => None.valid
    case Header.Modify(self, f, _) => apply(header = self, value).map(f)
