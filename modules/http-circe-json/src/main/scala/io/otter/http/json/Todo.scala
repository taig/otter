package io.otter.http.json

import io.taig.otter.http.Request
import io.taig.otter.Schema
import io.taig.otter as Base
import io.taig.otter.json.JsonDecoder
import io.circe.parser.parse
import io.circe.jawn.JawnParser
import io.taig.otter.Dsl
import io.circe.Json
import cats.syntax.all.*

trait JsonHttpSyntax extends Dsl:
  val binary: Request.Body.Singlepart.Strict[Nothing, Array[Byte]] =
    Request.Body.Singlepart.Strict.Binary

  private val parser = new JawnParser

  def json[A](schema: Schema.Reader[A]): Request.Body.Singlepart.Strict[container.Schema, A] =
    // TODO content type header must be present, perhaps I even have to use the encoding?
    Request.Body.Singlepart.Strict.Apply(
      parser = parser.parseByteArray(_).toOption.get, // TODO when where and how to handle errors?
      decoder = JsonDecoder,
      schema
    )

object Playground:
  import io.taig.otter.Plain.*

  // TODO dynamic????????
  val json: Dynamic[Json, Json] = Base.Dynamic.Root[Json, Json](_.valid, identity)

  val whatever: Dynamic[String, String] = Base.Dynamic.Root[String, String](_.valid, identity)
