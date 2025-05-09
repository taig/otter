package io.taig.otter

import io.taig.otter.http.FormData
import io.taig.otter.http.FormDataDsl
import io.taig.otter.http.HttpDsl
import io.taig.otter.http.HttpFormDataDsl
import io.taig.otter.http.HttpJsonDsl

object dsl extends CoreSyntax, HttpDsl, HttpFormDataDsl, HttpJsonDsl:
  object formData
      extends FormDataDsl,
        CaseInsensitiveDsl[FormData.Value.Primitive],
        JavaTimeDsl[FormData.Value.Primitive]

  object json extends JsonDsl, CaseInsensitiveDsl[Json.Primitive], JavaTimeDsl[Json.Primitive]
