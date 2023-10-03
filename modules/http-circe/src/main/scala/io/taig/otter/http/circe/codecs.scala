package io.taig.otter.http.circe

import cats.data.{Chain, Validated}
import io.circe.jawn.JawnParser
import io.circe.{Json, Printer}
import io.taig.otter.circe.{fromData, toData}
import io.taig.otter.http.syntax.{code, result}
import io.taig.otter.http.{syntax as http, App, MediaType, Request, Response, Result, Results, Routes}
import io.taig.otter.codecs.*
import io.taig.otter.validation.{Violation, Violations}
import io.taig.otter.{codecs, Codec, Dynamic}
import org.typelevel.ci.*

import java.nio.charset.StandardCharsets

object codecs:
  val json: Dynamic[Json] = dynamic.any.imap(fromData)(toData)

  private val parser = new JawnParser()

  object input:
    def json[A](codec: Codec[A], printer: Printer): Request.Body.Singlepart.Strict[A] =
      http.input(
        (_, bytes) =>
          Validated
            .fromEither(parser.parseByteArray(bytes))
            .leftMap(_ => Violations.rootNec(Violation.tpe("json")))
            .map(toData),
        data => (Chain.empty, printer.print(fromData(data)).getBytes(StandardCharsets.UTF_8)),
        codec
      )
    def json[A](codec: Codec[A]): Request.Body.Singlepart.Strict[A] = json(codec, Printer.noSpaces)

  object output:
    def json[A](codec: Codec[A], printer: Printer): Response.Body.Strict[A] = http.output(
      (_, bytes) =>
        Validated
          .fromEither(parser.parseByteArray(bytes))
          .leftMap(_ => Violations.rootNec(Violation.tpe("json")))
          .map(toData),
      data => (Chain.empty, printer.print(fromData(data)).getBytes(StandardCharsets.UTF_8)),
      codec,
      MediaType.application.json
    )
    def json[A](codec: Codec[A]): Response.Body.Strict[A] = json(codec, Printer.noSpaces)

  object response:
    def apply[A](results: Results[A]): Response[A] = Response(
      results,
      result(code.unprocessableEntity, output.json(violations))
        .description("The request body did not pass validation checks")
    )

    def apply[A](result: Result[A]): Response[A] = response(result.toResults)

  def app[F[_]](routes: Routes[F]): App[F] = App(
    routes,
    response(result(code.notFound)),
    response(result(code.internalServerError))
  )
