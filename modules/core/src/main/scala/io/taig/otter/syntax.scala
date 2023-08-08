package io.taig.otter

object syntax:
  extension [A](self: A) def asOpenApi(using Encoder[A]): OpenApi = Encoder[A].encode(self)

  extension (self: String) def :=[A: Encoder](value: A): (String, OpenApi) = (self, Encoder[A].encode(value))
