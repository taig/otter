package io.taig.otter

import io.taig.otter.component.CaseInsensitiveComponent
import io.taig.otter.component.JavaTimeComponent
import io.taig.otter.component.JsonComponent
import io.taig.otter.http.FormData
import io.taig.otter.http.component.FormDataComponent
import io.taig.otter.http.component.HttpHeaderComponent
import io.taig.otter.http.component.ParameterComponent
import io.taig.otter.http.syntax.AllHttpJsonSyntax
import io.taig.otter.http.syntax.AllHttpSyntax
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.http.Header
import io.taig.otter.http.Parameter

object dsl extends AllSyntax, AllHttpSyntax, AllHttpJsonSyntax:
  lazy val formData: FormDataComponent & CaseInsensitiveComponent[FormData.Value.Primitive] & JavaTimeComponent[
    FormData.Value.Primitive
  ] = new FormDataComponent
    with CaseInsensitiveComponent[FormData.Value.Primitive]
    with JavaTimeComponent[FormData.Value.Primitive]

  lazy val header: HttpHeaderComponent & CaseInsensitiveComponent[Header.Value] & JavaTimeComponent[Header.Value] =
    new HttpHeaderComponent with CaseInsensitiveComponent[Header.Value] with JavaTimeComponent[Header.Value]

  lazy val parameter
      : ParameterComponent & CaseInsensitiveComponent[Parameter.Value] & JavaTimeComponent[Parameter.Value] =
    new ParameterComponent with CaseInsensitiveComponent[Parameter.Value] with JavaTimeComponent[Parameter.Value]

  lazy val json: JsonComponent & CaseInsensitiveComponent[Json.Primitive] & JavaTimeComponent[Json.Primitive] =
    new JsonComponent with CaseInsensitiveComponent[Json.Primitive] with JavaTimeComponent[Json.Primitive]
