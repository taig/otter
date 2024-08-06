package io.taig.otter.http.json

import io.circe.jawn.JawnParser
import io.circe.Printer
import io.taig.otter.http as Http
import io.taig.otter.json.toData
import io.taig.otter.json.fromData
import java.nio.charset.StandardCharsets
import cats.data.Validated
import Http.MediaType

trait Codecs extends Http.Types, Http.Codecs:
  self =>

  private val parser = new JawnParser()

  def json[A](codec: Codec[A], printer: Printer): Body[A] = body(
    mediaType = MediaType.application.json,
    codec,
    (charset, bytes) =>
      // Jawn only supports byte decoding via UTF-8, so we have to decode a string first
      // for alternative encodings
      val result = charset match
        case Some(StandardCharsets.UTF_8) | None => parser.parseByteArray(bytes)
        case Some(charset)                       => parser.parse(new String(bytes, charset))

      Validated
        .fromEither(result)
        .leftMap(exception =>
          Violations.rootNec(Violation(Constraint.Type("json"), actual = Data.String(exception.getMessage)))
        )
        .map(toData)
    ,
    (charset, data) => printer.print(fromData(data)).getBytes(charset.getOrElse(StandardCharsets.UTF_8))
  )

  def json[A](codec: Codec[A]): Body[A] = json(codec, Printer.noSpaces)

object Codecs extends Codecs
