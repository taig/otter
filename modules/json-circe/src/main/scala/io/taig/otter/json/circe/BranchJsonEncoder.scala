package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import io.circe.JsonObject
import io.taig.otter.Sum.Discriminator
import io.circe.syntax.*

object BranchJsonEncoder:
  def apply[A](branch: Branch.Writer[A], disciminator: Sum.Discriminator, a: A): JsonObject = branch match
    case Base.Branch.Root(name, schema)        => root(name, schema, disciminator, a)
    case Base.Branch.Writer.Root(name, schema) => root(name, schema, disciminator, a)

  def root[A](name: String, schema: Schema.Writer[A], discriminator: Sum.Discriminator, a: A): JsonObject =
    discriminator match
      case Sum.Discriminator.Nested(identifier, value) =>
        JsonObject(identifier := name, value := JsonEncoder(schema, a))
      case Sum.Discriminator.Merged(identifier) =>
        JsonEncoder(schema, a).asObject.getOrElse(JsonObject.empty).add(identifier, name.asJson)
      case Sum.Discriminator.Keyed => JsonObject(name := JsonEncoder(schema, a))
