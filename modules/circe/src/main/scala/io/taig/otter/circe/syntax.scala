package io.taig.otter.circe

import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.http.Request
import io.taig.otter.schemas.*

object syntax:
  val json: Schema.Dynamic[Json] = dynamic.any.imap(fromData)(toData)

  object body:
    val json: Request.Body.Singlepart.Strict[Json] = ???
    def json[A](schema: Schema[A]): Request.Body.Singlepart.Strict[A] = ???
