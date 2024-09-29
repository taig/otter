package io.taig.otter.sample.app

import cats.effect.IO
import io.github.arainko.ducktape.*
import io.taig.otter.sample.Session
import io.taig.otter.sample.api.EndpointImplementation
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.UserApiSchema
import io.taig.otter.sample.app.repository.LibrarianRepository
import io.taig.otter.sample.app.transformers.given

final class SampleEndpointImplementation(librarian: LibrarianRepository) extends EndpointImplementation[IO]:
  override def findUser(session: SessionApiSchema): IO[Option[UserApiSchema]] = librarian
    .findBySession(session.to[Session])
    .map(_.map(_.to[LibrarianApiSchema]))

object SampleEndpointImplementation:
  def apply(librarian: LibrarianRepository): EndpointImplementation[IO] =
    new SampleEndpointImplementation(librarian)
