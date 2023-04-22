package io.taig.openapi

object syntax:
  extension [A](a: A) def asOpenApi(using encoder: Encoder[A]): OpenApi = encoder.encode(a)

  extension (key: String) def :=[A](a: A)(using Encoder[A]): (String, OpenApi) = (key, a.asOpenApi)
