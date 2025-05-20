package io.taig.otter.sample.app

import io.taig.otter.codec.ZodEndpointPrinter
import io.taig.otter.sample.api.endpoint

object SampleZodApp:
  @main
  def run = {
    val zod = ZodEndpointPrinter.print(endpoint.librarian.librarians.post)

    println(zod)
  }
