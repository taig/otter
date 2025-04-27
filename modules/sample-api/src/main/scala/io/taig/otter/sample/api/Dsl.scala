package io.taig.otter.sample.api

import cats.syntax.all.*
import io.taig.otter.http.HttpDsl
import io.taig.otter.JsonDsl
import io.taig.otter.CaseInsensitiveDsl
import io.taig.otter.Json
import io.taig.otter.Syntax

object Dsl extends HttpDsl, Syntax:
  object json extends JsonDsl, CaseInsensitiveDsl[Json.Primitive]
