package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.jawn.JawnParser
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.CirceJsonDecoder

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

object CirceJsonPayloadDecoder extends PayloadDecoder[Json]:
  val parser = new JawnParser()

  override def decode[A](codec: Json[A], charset: Option[Charset], bytes: Array[Byte]): Validated[Violations, A] =
    val result = charset match
      case Some(StandardCharsets.UTF_8) | None => parser.parseByteArray(bytes)
      case Some(charset)                       => parser.parse(new String(bytes, charset))

    result.toValidated
      .leftMap: failure =>
        Violation.fromConstraint(
          constraint = Constraint.Generic.Type(name = "json"),
          actual = "unknown",
          hint = failure.show.some
        )
      .leftMap(Violations.rootNec)
      .andThen(CirceJsonDecoder.decode(codec, _))
