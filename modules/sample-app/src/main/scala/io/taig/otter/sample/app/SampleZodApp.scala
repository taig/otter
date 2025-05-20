package io.taig.otter.sample.app

import io.taig.otter.codec.ZodEndpointRenderer
import io.taig.otter.sample.api.endpoint
import io.taig.otter.codec.ZodEndpointsRenderer

object SampleZodApp:
  @main
  def run = {
    val zod = ZodEndpointsRenderer(imports = Nil).render(
      endpoints = List(
        endpoint.librarian.librarians.post,
        endpoint.librarian.librarians.reference.get
      )
    )

    println(zod)
  }
