package io.taig.otter.sample.api

import io.taig.otter.Dsl.json.*
import io.taig.otter.Dsl.*
import io.taig.otter.Json

final case class BookApiSchema(author: String, genre: GenreApiSchema, isbn: String, title: String)

object BookApiSchema:
  val json: Json.Record[BookApiSchema] = (
    field("author", string) :*
    field("genre", GenreApiSchema.json) :*
    field("isbn", string) :*
    field("title", string)
  ).to