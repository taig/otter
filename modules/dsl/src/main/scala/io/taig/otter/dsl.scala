package io.taig.otter

import io.taig.otter.http.HttpDsl
import io.taig.otter.http.FormDataDsl

object dsl extends CoreSyntax, HttpDsl, HttpJsonDsl:
  object form extends FormDataDsl

  object json extends JsonDsl, CaseInsensitiveDsl[Json.Primitive], JavaTimeDsl[Json.Primitive]
