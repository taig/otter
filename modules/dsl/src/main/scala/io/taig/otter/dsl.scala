package io.taig.otter

import io.taig.otter.http.HttpDsl
import io.taig.otter.http.FormDataDsl
import io.taig.otter.http.FormData

object dsl extends CoreSyntax, HttpDsl, HttpJsonDsl:
  object formData
      extends FormDataDsl,
        CaseInsensitiveDsl[FormData.Value.Primitive],
        JavaTimeDsl[FormData.Value.Primitive]

  object json extends JsonDsl, CaseInsensitiveDsl[Json.Primitive], JavaTimeDsl[Json.Primitive]
