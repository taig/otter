package io.taig.otter.sample.api

import io.taig.otter.Json
import io.taig.otter.component.CaseInsensitiveComponent
import io.taig.otter.component.JavaTimeComponent
import io.taig.otter.component.JsonComponent
import io.taig.otter.http.FormData
import io.taig.otter.http.Header
import io.taig.otter.http.Parameter
import io.taig.otter.http.component.FormDataComponent
import io.taig.otter.http.component.HttpHeaderComponent
import io.taig.otter.http.component.ParameterComponent
import io.taig.otter.http.syntax.AllHttpJsonSyntax
import io.taig.otter.http.syntax.AllHttpSyntax
import io.taig.otter.syntax.AllSyntax

object dsl:
  export AllSyntax.*
  export AllHttpSyntax.*
  export AllHttpJsonSyntax.*

  val formData: FormDataComponent & CaseInsensitiveComponent[FormData.Schema.Primitive] & JavaTimeComponent[
    FormData.Schema.Primitive
  ] = new FormDataComponent
    with CaseInsensitiveComponent[FormData.Schema.Primitive]
    with JavaTimeComponent[FormData.Schema.Primitive]

  val header: HttpHeaderComponent & CaseInsensitiveComponent[Header.Schema] & JavaTimeComponent[Header.Schema] =
    new HttpHeaderComponent with CaseInsensitiveComponent[Header.Schema] with JavaTimeComponent[Header.Schema]

  val parameter: ParameterComponent & CaseInsensitiveComponent[Parameter.Schema] & JavaTimeComponent[Parameter.Schema] =
    new ParameterComponent with CaseInsensitiveComponent[Parameter.Schema] with JavaTimeComponent[Parameter.Schema]

  val json: JsonComponent & CaseInsensitiveComponent[Json.Primitive] & JavaTimeComponent[Json.Primitive] =
    new JsonComponent with CaseInsensitiveComponent[Json.Primitive] with JavaTimeComponent[Json.Primitive]
