package io.taig.otter.sample.app

import io.taig.otter.codec.TypescriptZodEndpointsRenderer
import io.taig.otter.sample.api.endpoint
import io.taig.otter.codec.JsonTypescriptRenderer
import io.taig.otter.TypescriptState
import io.taig.otter.codec.TypescriptZodEncoder
import io.taig.otter.ZodState
import cats.syntax.all.*
import io.taig.otter.dsl.json.*
import io.taig.otter.Json
import io.taig.otter.Data
import io.taig.otter.Keys.*

object SampleZodApp:
  object data:
    val primitive: Json[Int | Boolean | String] = (int | boolean | string).metadata(name, "Primitive")

    val any: Json.Nullable[Int | Boolean | String | Data.Null] = primitive.nullable
      .imap(_.getOrElse(Data.Null)) {
        case Data.Null        => None
        case data: (Int | Boolean | String) => Some(data)
      }
      .metadata(name, "Any")
      
  @main
  def run = {
    val zod = TypescriptZodEndpointsRenderer(imports = Nil).render(
      endpoints = List(
        endpoint.librarian.librarians.reference.get,
        endpoint.librarian.post
      )
    )

    println(zod)
  }
