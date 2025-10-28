package io.taig.otter.sample.api

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.taig.otter.Dsl.*
import io.taig.otter.Dsl.json.*
import io.taig.otter.Json

type IsbnApiSchema = String :| FixedLength[13]

object IsbnApiSchema:
  val json: Json.Primitive.String[IsbnApiSchema] = iron.text[FixedLength[13]](string)
