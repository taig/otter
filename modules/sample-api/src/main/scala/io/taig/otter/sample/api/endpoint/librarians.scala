package io.taig.otter.sample.api.endpoint

import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.api.Role
import io.taig.otter.sample.api.AuthenticatedEndpoint

object librarians:
  val url: Url[Unit] = __ / "librarians"

  object self:
    val url: Url[Unit] = librarians.url / "self"

    object sessions:
      val url: Url[Unit] = self.url / "sessions"

      object post:
        enum Error:
          case EmailOrPasswordIncorrect

        object Error:
          val results: Results[Error] =
            val emailOrPasswordIncorrect: Codec[EmailOrPasswordIncorrect.type] =
              error("emailOrPasswordIncorrect", singleton(EmailOrPasswordIncorrect))

            result(code.unauthorized, json.output(emailOrPasswordIncorrect)).toResults.to

        def apply(): AuthenticatedEndpoint[Role.Guest, LibrarianApiSchema.Login, Either[Error, SessionApiSchema]] =
          endpoint(
            request(method.post, url, json.input(LibrarianApiSchema.Login.codec)),
            response(Error.results :+ result(code.created, json.output(SessionApiSchema.codec)))
          ).summary("Create librarian session")
            .description(
              "For security reasons, this endpoint does not give away whether the given email address is " +
                "unknown or the given password is incorrect."
            )
            .operationId("createLibrarianSession")
            .tags("librarians")
            .role(Role.Guest)
