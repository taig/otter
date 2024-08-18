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

  override def response[A](results: Results[A]): Response[A] = Http.Response(
    results,
    error = (
      result(code.notAcceptable, text(route.error.text.contentNegotiationFailed) + json(route.error.contentNegotiationFailed)) :+
        result(code.unsupportedMediaTypes, text(route.error.text.mediaTypesUnsupported) + json(route.error.mediaTypesUnsupported)) :+
        result(code.unprocessableEntity, text(route.error.text.validationViolations) + json(route.error.validationViolations))
    ).to,
    failure = result(code.internalServerError)
  )

object Codecs extends Codecs
