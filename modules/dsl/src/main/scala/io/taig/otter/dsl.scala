package io.taig.otter

import io.taig.otter.http.HttpDsl

object dsl extends CoreSyntax, HttpDsl, HttpJsonDsl:
  object json extends JsonDsl, CaseInsensitiveDsl[Json.Primitive], JavaTimeDsl[Json.Primitive]
