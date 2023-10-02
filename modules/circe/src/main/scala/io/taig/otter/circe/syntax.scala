package io.taig.otter.circe

import io.circe.JsonObject

object syntax:
  extension (self: JsonObject)
    def ++(obj: JsonObject): JsonObject = JsonObject.fromIterable(self.toIterable ++ obj.toIterable)
    def dropNullValues: JsonObject = self.filter { case (_, value) => !value.isNull }
