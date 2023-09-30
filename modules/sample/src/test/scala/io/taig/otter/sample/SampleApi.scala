package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.data.Librarian

final class SampleApi(client: SampleClient):
  def librarian: IO[Librarian.Session] = client.submitSuccess(
    endpoints.librarians.self.sessions.post,
    session = None,
    Librarian.Create.Default.toLogin
  )
