package io.taig.otter.json

import io.circe.Json
import io.circe.JsonObject
import io.taig.otter.*
import io.circe.syntax.*

object BranchJsonEncoder:
  def apply[A](branch: Branch[?, A], discriminator: Discriminator, a: A): JsonObject = branch match
    case Branch.Root(_, name, schema) =>
      discriminator match
        case Discriminator.Nested(identifier, value) =>
          JsonObject(identifier := name, value := JsonEncoder(schema, a))
        case Discriminator.Merged(identifier) =>
          JsonEncoder(schema, a).asObject.getOrElse(JsonObject.empty).add(identifier, name.asJson)
        case Discriminator.Keyed => JsonObject(name := JsonEncoder(schema, a))
    case Branch.Transform(self, _, f) => BranchJsonEncoder(self, discriminator, f(a))
