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
          bytes =>
            Validated
              .fromEither(parser.parseByteArray(bytes))
              .leftMap(exception =>
                Violations.rootNec(Violation(Constraint.Type("json"), actual = Data.String(exception.getMessage)))
              )
              .map(toData),
          data => printer.print(fromData(data)).getBytes(StandardCharsets.UTF_8),
          codec
        )

      def apply[A](codec: Codec[A]): Request.Body[A] = apply(codec, Printer.noSpaces)

    object output:
      def apply[A](codec: Codec[A]): Response.Body[A] = ???

object Codecs extends Codecs
