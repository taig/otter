package io.taig.otter.sample.app

import io.taig.otter.codec.TypescriptZodEndpointsRenderer
import io.taig.otter.sample.api.endpoint
import cats.syntax.all.*
import io.taig.otter.dsl.json.*
import io.taig.otter.Json
import io.taig.otter.Data
import io.taig.otter.Keys.*

object SampleZodApp:
  @main
  def run = {
    val zod = TypescriptZodEndpointsRenderer(imports = Nil).render(
      endpoints = List(
        endpoint.librarian.librarians.reference.get,
        endpoint.librarian.post,
        endpoint.librarian.librarians.reference.get
      )
    )

    println(zod)
  }
