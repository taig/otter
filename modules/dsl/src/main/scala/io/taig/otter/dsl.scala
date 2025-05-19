package io.taig.otter

import io.taig.otter.component.CaseInsensitiveComponent
import io.taig.otter.component.JavaTimeComponent
import io.taig.otter.component.JsonComponent
import io.taig.otter.http.FormData
import io.taig.otter.http.Http
import io.taig.otter.http.component.FormDataComponent
import io.taig.otter.http.component.HttpParameterComponent
import io.taig.otter.http.syntax.AllHttpJsonSyntax
import io.taig.otter.http.syntax.AllHttpSyntax
import io.taig.otter.syntax.AllSyntax

object dsl extends AllSyntax, AllHttpSyntax, AllHttpJsonSyntax:
  lazy val formData: FormDataComponent & CaseInsensitiveComponent[FormData.Value.Primitive] & JavaTimeComponent[
    FormData.Value.Primitive
  ] =
    new FormDataComponent
      with CaseInsensitiveComponent[FormData.Value.Primitive]
      with JavaTimeComponent[FormData.Value.Primitive]

  lazy val parameter
      : HttpParameterComponent & CaseInsensitiveComponent[Http.Parameter] & JavaTimeComponent[Http.Parameter] =
    new HttpParameterComponent with CaseInsensitiveComponent[Http.Parameter] with JavaTimeComponent[Http.Parameter]

  lazy val json: JsonComponent & CaseInsensitiveComponent[Json.Primitive] & JavaTimeComponent[Json.Primitive] =
    new JsonComponent with CaseInsensitiveComponent[Json.Primitive] with JavaTimeComponent[Json.Primitive]
