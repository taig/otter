package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.circe.jawn.JawnParser
import io.taig.otter.CirceJsonDecoder
import io.taig.otter.Json
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import org.typelevel.ci.*

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

final class CirceJsonPayloadDecoder extends PayloadDecoder[Json]:
  val parser = new JawnParser()

  override def apply[A](codec: Json[A], contentType: MediaType, bytes: Array[Byte]): Validated[Violations, A] =
    val charset = contentType.parameters
      .get(ci"charset")
      .headOption
      .flatMap(value =>
        try Charset.forName(value).some
        catch { case _: IllegalArgumentException => none }
      )
      .getOrElse(StandardCharsets.UTF_8)

    val result =
      if charset == StandardCharsets.UTF_8
      then parser.parseByteArray(bytes)
      else parser.parse(new String(bytes, charset))

    result.toValidated
      .leftMap: failure =>
        Violations.rootNec(Violation.tpe(name = "json", actual = "unknown", hint = failure.show))
      .andThen(CirceJsonDecoder(codec, _))

object CirceJsonPayloadDecoder:
  val Default: PayloadDecoder[Json] = new CirceJsonPayloadDecoder()
