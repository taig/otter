package io.taig.otter.circe

import io.circe.Json
import io.taig.otter.{schemas, Schema}
import io.taig.otter.http.syntax.{code, result}
import io.taig.otter.http.{App, Request, Response, Result, Results, Routes}
import io.taig.otter.schemas.*

object syntax:
  val json: Schema.Dynamic[Json] = dynamic.any.imap(fromData)(toData)

  object input:
    val json: Request.Body.Singlepart.Strict[Json] = ???
    def json[A](schema: Schema[A]): Request.Body.Singlepart.Strict[A] = ???

  object output:
    val json: Response.Body.Strict[Json] = ???
    def json[A](schema: Schema[A]): Response.Body.Strict[A] = ???

  object response:
    def apply[A](results: Results[A]): Response[A] = Response(
      results,
      result(code.unprocessableEntity, output.json(schemas.violations))
    )

    def apply[A](result: Result[A]): Response[A] = response(result.toResults)

  def app[F[_]](routes: Routes[F]): App[F] = App(
    routes,
    response(result(code.notFound)),
    response(result(code.internalServerError))
  )
