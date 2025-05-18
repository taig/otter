package io.taig.otter.sample.route.librarian.librarians.reference

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import io.circe.Printer
import io.taig.otter.dsl.*
import io.taig.otter.http.AppClient
import io.taig.otter.munit.OtterEffectSuite
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.LibrarianReferenceUnknown
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.app.SampleApp
import munit.CatsEffectSuite
import org.http4s.SameSite
import org.typelevel.ci.*

import java.util.UUID

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
