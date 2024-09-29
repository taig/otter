package io.taig.otter.sample

import cats.effect.IO
import io.github.arainko.ducktape.*
import io.taig.otter.munit.Assertions
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.api.schema.SessionApiSchema
import munit.Location

final class SampleApi(client: SampleClient) extends Assertions:
  def librarian(using Location): IO[SessionApiSchema] = client
    .fallible(
      endpoint.librarians.self.sessions.post(),
      session = None,
      Librarian.Create.Default.toLibrarianLogin.to[LibrarianApiSchema.Login]
    )
    .assertSuccess
