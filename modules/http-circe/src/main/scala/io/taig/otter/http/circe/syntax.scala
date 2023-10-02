package io.taig.otter.http.circe

import cats.data.{Chain, Validated}
import io.circe.jawn.JawnParser
import io.circe.{Json, Printer}
import io.taig.otter.circe.{fromData, toData}
import io.taig.otter.http.syntax.{code, result}
import io.taig.otter.http.{syntax as http, App, Request, Response, Result, Results, Routes}
import io.taig.otter.codecs.*
import io.taig.otter.validation.{Violation, Violations}
import io.taig.otter.{codecs, Codec, Dynamic}

import java.nio.charset.StandardCharsets

object syntax:
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
    def json(printer: Printer): Response.Body.Strict[Json] = http.output.binary.andThen { bytes =>
      Validated
        .fromEither(parser.parseByteArray(bytes))
        .leftMap(_ => Violations.rootNec(Violation.tpe("json")))
    }(printer.print(_).getBytes(StandardCharsets.UTF_8))
    val json: Response.Body.Strict[Json] = json(Printer.noSpaces)
    def json[A](codec: Codec[A]): Response.Body.Strict[A] = http.output(json.imap(toData)(fromData), codec)

  object response:
    def apply[A](results: Results[A]): Response[A] =
      Response(results, result(code.unprocessableEntity, output.json(codecs.violations)))

    def apply[A](result: Result[A]): Response[A] = response(result.toResults)

  def app[F[_]](routes: Routes[F]): App[F] = App(
    routes,
    response(result(code.notFound)),
    response(result(code.internalServerError))
  )
