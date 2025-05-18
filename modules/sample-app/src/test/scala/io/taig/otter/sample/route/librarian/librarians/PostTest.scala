package io.taig.otter.sample.route.librarian.librarians

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
