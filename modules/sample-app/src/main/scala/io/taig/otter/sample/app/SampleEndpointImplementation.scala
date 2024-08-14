package io.taig.otter.sample.app

import io.taig.otter.sample.api.EndpointImplementation
import cats.effect.IO
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.UserApiSchema
import io.taig.otter.sample.app.repository.LibrarianRepository
import io.taig.otter.sample.app.conversion.toLibrarianApiSchema

final class SampleEndpointImplementation(librarian: LibrarianRepository) extends EndpointImplementation[IO]:
  override def findUser(session: SessionApiSchema): IO[Option[UserApiSchema]] = librarian
    .findBySession(conversion.toSession(session))
    .map(_.map(toLibrarianApiSchema))

object SampleEndpointImplementation:
  def apply(librarian: LibrarianRepository): EndpointImplementation[IO] =
    new SampleEndpointImplementation(librarian)
