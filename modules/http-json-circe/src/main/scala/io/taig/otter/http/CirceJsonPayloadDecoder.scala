package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.circe.jawn.JawnParser
import io.taig.otter.CirceJsonDecoder
import io.taig.otter.Json
import io.taig.otter.Violation
import io.taig.otter.Violations

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

final class CirceJsonPayloadDecoder extends PayloadDecoder[Json]:
  val parser = new JawnParser()

  override def apply[A](codec: Json[A], charset: Option[Charset], bytes: Array[Byte]): Validated[Violations, A] =
    val result = charset match
      case Some(StandardCharsets.UTF_8) | None => parser.parseByteArray(bytes)
      case Some(charset)                       => parser.parse(new String(bytes, charset))

    result.toValidated
      .leftMap: failure =>
        Violations.rootNec(Violation.tpe(name = "json", actual = "unknown", hint = failure.show))
      .andThen(CirceJsonDecoder(codec, _))

object CirceJsonPayloadDecoder:
  val Default: PayloadDecoder[Json] = new CirceJsonPayloadDecoder()
