package io.taig.otter.circe

import cats.data.Validated
import io.circe.jawn.JawnParser
import io.circe.{Json, Printer}
import io.taig.otter.http.syntax.{code, result}
import io.taig.otter.http.{syntax as http, App, Request, Response, Result, Results, Routes}
import io.taig.otter.schemas.*
import io.taig.otter.validation.{Violation, Violations}
import io.taig.otter.{schemas, Dynamic, Schema}

import java.nio.charset.StandardCharsets

object syntax:
  val json: Dynamic[Json] = dynamic.any.imap(fromData)(toData)

  private val parser = new JawnParser()

  object input:
    def json(printer: Printer): Request.Body.Singlepart.Strict[Json] =
      http.input.binary.andThen { bytes =>
        Validated
          .fromEither(parser.parseByteArray(bytes))
          .leftMap(_ => Violations.rootNec(Violation.tpe("json")))
      }(printer.print(_).getBytes(StandardCharsets.UTF_8))
    val json: Request.Body.Singlepart.Strict[Json] = json(Printer.noSpaces)
    def json[A](schema: Schema[A]): Request.Body.Singlepart.Strict[A] = http.input(json.imap(toData)(fromData), schema)

  object output:
    def json(printer: Printer): Response.Body.Strict[Json] =
      http.output.binary.imap(_ => ???)(printer.print(_).getBytes(StandardCharsets.UTF_8))
    val json: Response.Body.Strict[Json] = json(Printer.noSpaces)
    def json[A](schema: Schema[A]): Response.Body.Strict[A] = http.output(json.imap(toData)(fromData), schema)

  object response:
    def apply[A](results: Results[A]): Response[A] =
      Response(results, result(code.unprocessableEntity, output.json(schemas.violations)))

    def apply[A](result: Result[A]): Response[A] = response(result.toResults)

  def app[F[_]](routes: Routes[F]): App[F] = App(
    routes,
    response(result(code.notFound)),
    response(result(code.internalServerError))
  )
