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

import java.nio.charset.StandardCharsets

trait codecs:
  val json: Dynamic[Json] = dynamic.any.imap(fromData)(toData)

  def app[F[_]](routes: Routes[F]): App[F] = App(
    routes,
    codecs.response(result(code.notFound)),
    codecs.response(result(code.internalServerError))
  )

object codecs extends codecs:
  private val parser = new JawnParser()

  trait input:
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

  object input extends input

  trait output:
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

  object output extends output

  trait response:
    def apply[A](results: Results[A]): Response[A] = http.response(results, output.json(violations))
    def apply[A](result: Result[A]): Response[A] = response(result.toResults)
    def apply[A, B](errors: Results[A], success: Result[B]): Response[Either[A, B]] = response(errors :+ success)

  object response extends response
