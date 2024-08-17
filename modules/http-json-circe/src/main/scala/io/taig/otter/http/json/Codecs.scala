package io.taig.otter.http.json

import io.circe.jawn.JawnParser
import io.circe.Printer
import io.taig.otter.http as Http
import io.taig.otter.json.toData
import io.taig.otter.json.fromData
import java.nio.charset.StandardCharsets
import cats.data.Validated
import java.nio.charset.Charset

trait Codecs extends Http.Types, Http.Codecs:
  self =>

  private val parser = new JawnParser()

  def json[A](
      codec: Codec[A],
      printer: Printer = Printer.noSpaces,
      fallback: => Charset = StandardCharsets.UTF_8
  ): Body[A] = body(
    mediaType = mediaType.application.json,
    codec,
    (charset, bytes) =>
      // Jawn only supports byte decoding via UTF-8, so we have to decode a string first
      // for alternative encodings
      val result = charset.getOrElse(fallback) match
        case StandardCharsets.UTF_8 => parser.parseByteArray(bytes)
        case charset                => parser.parse(new String(bytes, charset))

      Validated
        .fromEither(result)
        .leftMap(exception => Violations.rootNec(Violation.tpe("json", actual = exception.getMessage)))
        .map(toData)
    ,
    (charset, data) => printer.print(fromData(data)).getBytes(charset.getOrElse(fallback))
  )

  val textOrformDataOrJsonViolations: Bodies[Violations] = textViolations + json(violations.nested)

  override def response[A](results: Results[A]): Response[A] = response(results, textOrformDataOrJsonViolations)

  override def response[A](result: Result[A]): Response[A] = response(result, textOrformDataOrJsonViolations)

object Codecs extends Codecs
