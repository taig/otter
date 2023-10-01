package io.taig.otter.openapi

import io.circe.{Json, JsonObject}

object syntax:
  extension (self: JsonObject)
    def ++(obj: JsonObject): JsonObject = JsonObject.fromIterable(self.toIterable ++ obj.toIterable)
    def dropNullValues: JsonObject = self.filter { case (_, value) => !value.isNull }

  extension [A](self: A)
    def withExtensions(extensions: Extensions): Extended[A] = Extended(self, extensions)
    def withExtensions(values: (String, Json)*): Extended[A] = Extended(self, Extensions(values*))
    def withoutExtensions: Extended[A] = withExtensions(Extensions.Empty)
