package io.taig.otter.http.json

import cats.data.Validated
import io.circe.Printer
import io.circe.jawn.JawnParser
import io.taig.otter.http as Http
import io.taig.otter.json.fromData
import io.taig.otter.json.toData

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

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

  override def app[F[_]](routes: Routes[F]): App[F] = App(
    routes,
    error = result(
      code.notFound,
      text(error.text.routeNotFound) + json(error.routeNotFound)
    ).toResults.to[App.Error]
  )

  override def response[A](results: Results[A]): Response[A] = Http.Response(
    results,
    errors = (
      result(
        code.notAcceptable,
        text(error.text.contentNegotiationFailed) + json(error.contentNegotiationFailed)
      ) :+ result(
        code.unsupportedMediaTypes,
        text(error.text.mediaTypesUnsupported) + json(error.mediaTypesUnsupported)
      ) :+ result(
        code.unprocessableEntity,
        text(error.text.validationViolations) + json(error.validationViolations)
      )
    ).to,
    failure = result(code.internalServerError)
  )

object Codecs extends Codecs
