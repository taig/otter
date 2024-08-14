package io.taig.otter.sample.app

import io.taig.otter.sample.api.EndpointImplementation
import cats.effect.IO
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.UserApiSchema
import io.taig.otter.sample.app.repository.LibrarianRepository
import io.github.arainko.ducktape.*
import io.taig.otter.sample.Session
import io.taig.otter.sample.app.transformers.given
import io.taig.otter.sample.api.schema.LibrarianApiSchema

final class SampleEndpointImplementation(librarian: LibrarianRepository) extends EndpointImplementation[IO]:
  override def findUser(session: SessionApiSchema): IO[Option[UserApiSchema]] = librarian
    .findBySession(session.to[Session])
    .map(_.map(_.to[LibrarianApiSchema]))

object SampleEndpointImplementation:
  def apply(librarian: LibrarianRepository): EndpointImplementation[IO] =
    new SampleEndpointImplementation(librarian)
