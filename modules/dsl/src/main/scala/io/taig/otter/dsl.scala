package io.taig.otter

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

object dsl extends AllSyntax, AllHttpSyntax, AllHttpJsonSyntax:
  lazy val formData: FormDataComponent & CaseInsensitiveComponent[FormData.Schema.Primitive] & JavaTimeComponent[
    FormData.Schema.Primitive
  ] = new FormDataComponent
    with CaseInsensitiveComponent[FormData.Schema.Primitive]
    with JavaTimeComponent[FormData.Schema.Primitive]

  lazy val header: HttpHeaderComponent & CaseInsensitiveComponent[Header.Schema] & JavaTimeComponent[Header.Schema] =
    new HttpHeaderComponent with CaseInsensitiveComponent[Header.Schema] with JavaTimeComponent[Header.Schema]

  lazy val parameter
      : ParameterComponent & CaseInsensitiveComponent[Parameter.Schema] & JavaTimeComponent[Parameter.Schema] =
    new ParameterComponent with CaseInsensitiveComponent[Parameter.Schema] with JavaTimeComponent[Parameter.Schema]

  lazy val json: JsonComponent & CaseInsensitiveComponent[Json.Primitive] & JavaTimeComponent[Json.Primitive] =
    new JsonComponent with CaseInsensitiveComponent[Json.Primitive] with JavaTimeComponent[Json.Primitive]
