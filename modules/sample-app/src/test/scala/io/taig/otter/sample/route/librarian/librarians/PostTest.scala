package io.taig.otter.sample.route.librarian.librarians

import cats.effect.IO
import io.taig.otter.http.LocalClient
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.app.SampleRoutes
import munit.CatsEffectSuite
import org.typelevel.ci.*

final class PostTest extends CatsEffectSuite:
  test("POST /librarian/librarians"):
    val client = LocalClient(SampleRoutes())
    client.submit(
      endpoint.librarian.librarians.post,
      contentType = None,
      LibrarianApiSchema.Create(email = ci"foobar@acme.com", password = "password")
    )
