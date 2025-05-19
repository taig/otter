package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Branch
import io.taig.otter.Discriminator
import io.taig.otter.Json
import io.taig.otter.Key

final class CirceJsonBranchEncoder(discriminator: Discriminator) extends Encoder[Json.Branch, CirceJson]:
  override def encode[A](schema: Json.Branch[A], a: A): CirceJson = encode(schema = schema.self, a)

  def encode[A](schema: Branch[Key, Json, A], a: A): CirceJson = schema match
    case Branch.Modify(self, _, g) => encode(schema = self, g(a))
    case Branch.Root(key, schema, _) =>
      discriminator match
        case Discriminator.Explicit(identifier, value) =>
          CirceJson.obj(
            identifier := ReferenceConstantRenderer(encoder = KeyPrinter.Unquoted).render(key),
            value := CirceJsonEncoder.encode(schema = schema.value, a)
          )
        case Discriminator.Merged(identifier) =>
          CirceJsonEncoder
            .encode(schema = schema.value, a)
            .deepMerge(
              CirceJson.obj(
                identifier := ReferenceConstantRenderer(encoder = KeyPrinter.Unquoted).render(key)
              )
            )
        case Discriminator.Keyed =>
          CirceJson.obj(
            ReferenceConstantRenderer(encoder = KeyPrinter.Unquoted).render(key) :=
              CirceJsonEncoder.encode(schema = schema.value, a)
          )
