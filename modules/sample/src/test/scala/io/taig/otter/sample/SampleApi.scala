package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.munit.OtterExtensions
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.data.Librarian

final class SampleApi(client: SampleClient) extends SampleExtensions:
  val librarian: IO[Librarian.Session] = client
    .submit(
      endpoints.librarians.self.sessions.post,
      Librarian.Create.Default.toLogin
    )
    .toSuccess
