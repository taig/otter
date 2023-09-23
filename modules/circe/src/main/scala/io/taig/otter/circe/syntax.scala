package io.taig.otter.circe

import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.http.{Request, Response}
import io.taig.otter.schemas.*

object syntax:
  val json: Schema.Dynamic[Json] = dynamic.any.imap(fromData)(toData)

  object input:
    val json: Request.Body.Singlepart.Strict[Json] = ???
    def json[A](schema: Schema[A]): Request.Body.Singlepart.Strict[A] = ???

  object output:
    val json: Response.Body.Strict[Json] = ???
    def json[A](schema: Schema[A]): Response.Body.Strict[A] = ???
