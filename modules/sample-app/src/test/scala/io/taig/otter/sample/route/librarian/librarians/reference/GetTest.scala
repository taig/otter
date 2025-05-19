package io.taig.otter.sample.route.librarian.librarians.reference

import cats.effect.IO
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.LibrarianReferenceUnknown

final class GetTest extends SampleSuite:
  client.test(endpoint.librarian.librarians.reference.get, "librarian reference unknown"): client =>
    for
      reference <- IO.randomUUID
      obtained <- client
        .submit(endpoint.librarian.librarians.reference.get)(input = reference)
        .assertError
    yield {
      assertEq(obtained, expected = LibrarianReferenceUnknown(reference))
    }
