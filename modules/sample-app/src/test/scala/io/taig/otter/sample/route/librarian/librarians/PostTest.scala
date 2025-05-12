package io.taig.otter.sample.route.librarian.librarians

import cats.effect.IO
import io.circe.Printer
import io.taig.otter.http.CirceJsonPayloadDecoder
import io.taig.otter.http.CirceJsonPayloadEncoder
import io.taig.otter.http.LocalClient
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.app.SampleRoutes
import munit.CatsEffectSuite
import org.typelevel.ci.*

final class PostTest extends CatsEffectSuite:
  test("POST /librarian/librarians"):
    val client = LocalClient(
      decoder = CirceJsonPayloadDecoder.Default,
      encoder = CirceJsonPayloadEncoder(printer = Printer.noSpaces),
      debug = true
    )(app(routes = SampleRoutes()))

    client.submit(
      endpoint.librarian.librarians.post,
      contentType = None,
      LibrarianApiSchema.Create(email = ci"foobar@acme.com", password = "password")
    )
