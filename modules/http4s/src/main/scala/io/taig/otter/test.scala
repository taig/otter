package io.taig.otter

import io.circe.Json
import io.taig.otter.http.Request
import io.taig.otter.schema.Schema

object test:
  val strict: Request.Body.Singlepart.Strict[Array[Byte]] = ???
  // TODO encoding
  val string: Request.Body.Singlepart.Strict[String] = strict.imap(new String(_))(_.getBytes())
  val json: Request.Body.Singlepart.Strict[Json] = string.ivalidate(???)(_.noSpaces)
  def json[A](schema: Schema[A]): Request.Body.Singlepart.Strict[A] =
    json.andThen(CirceDecoder.schema.decode(schema, _))(CirceEncoder.schema.encode(schema, _).getOrElse(Json.Null))
