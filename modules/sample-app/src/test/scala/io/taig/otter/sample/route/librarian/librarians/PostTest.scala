package io.taig.otter.sample.route.librarian.librarians

import cats.effect.IO
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import org.typelevel.ci.*

final class PostTest extends SampleSuite:
  client.test(endpoint.librarian.librarians.post): client =>
    for
      obtained <- client
        .submit(endpoint.librarian.librarians.post)(
          input = LibrarianApiSchema.Create(email = ci"foobar@acme.com", password = "password")
        )
        .assertSuccess
      expected <- client
        .submit(endpoint.librarian.librarians.reference.get)(input = obtained.reference)
        .assertSuccess
    yield {
      assertEq(obtained, expected)
    }
