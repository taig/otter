package io.taig.otter

import io.taig.otter.http.HttpDsl

object dsl extends HttpDsl, HttpJsonDsl, ComparisonDsl:
  object json extends JsonDsl, CaseInsensitiveDsl[Json.Primitive], JavaTimeDsl[Json.Primitive]
