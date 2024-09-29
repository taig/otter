package io.taig.otter.sample.api.endpoint

import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.sample.api.Role
import io.taig.otter.sample.api.RoleEndpoint
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.api.schema.SessionApiSchema

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
          val results: Results[Error] = result(
            code.unauthorized,
            json(error("emailOrPasswordIncorrect").as(Error.EmailOrPasswordIncorrect))
          ).toResults.to

        def apply(): RoleEndpoint[Role.Guest, LibrarianApiSchema.Login, Either[Error, SessionApiSchema]] =
          endpoint(
            request(method.post, url, json(LibrarianApiSchema.Login.codec)),
            response(Error.results :+ result(code.created, json(SessionApiSchema.codec)))
          ).summary("Create librarian session")
            .description(
              "For security reasons, this endpoint does not give away whether the given email address is " +
                "unknown or the given password is incorrect."
            )
            .operationId("createLibrarianSession")
            .tags("librarians")
            .role(Role.Guest)
