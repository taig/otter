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

  object json:
    object input:
      def apply[A](codec: Codec[A], printer: Printer): Request.Body[A] =
        self.input(
          MediaType.application.json,
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

      def apply[A](codec: Codec[A]): Request.Body[A] = apply(codec, Printer.noSpaces)

    object output:
      def apply[A](codec: Codec[A]): Response.Body[A] = ???

object Codecs extends Codecs
