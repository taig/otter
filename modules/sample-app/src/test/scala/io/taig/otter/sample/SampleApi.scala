package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.github.arainko.ducktape.*
import io.taig.otter.sample.api.schema.SessionApiSchema

final class SampleApi(client: SampleClient) extends SampleSyntax:
  val librarian: IO[SessionApiSchema] = client
    .fallible(
      endpoint.librarians.self.sessions.post(),
      session = None,
      Librarian.Create.Default.toLibrarianLogin.to[LibrarianApiSchema.Login]
    )
    .toSuccess
