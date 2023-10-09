package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.data.{Librarian, Session}

final class SampleApi(client: SampleClient) extends SampleExtensions:
  // TODO can we use assertions here as well?
  val librarian: IO[Session] = client
    .submit(
      endpoints.librarians.self.sessions.post,
      Librarian.Create.Default.toLogin
    )
    .toSuccess
