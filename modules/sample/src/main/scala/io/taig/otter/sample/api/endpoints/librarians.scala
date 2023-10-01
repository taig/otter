package io.taig.otter.sample.api.endpoints

import io.taig.otter.Schema
import io.taig.otter.dsl.*
import io.taig.otter.http.{Endpoint as OtterEndpoint, Results, Url}
import io.taig.otter.sample.api.{schemas, Role}
import io.taig.otter.sample.data.Librarian

object librarians:
  val url: Url[Unit] = __ / "librarians"

  object self:
    val url: Url[Unit] = librarians.url / "self"

    object sessions:
      val url: Url[Unit] = self.url / "sessions"

      enum Post:
        case EmailOrPasswordIncorrect

      object Post:
        val results: Results[Post] =
          val emailOrPasswordIncorrect: Schema[EmailOrPasswordIncorrect.type] =
            error("emailOrPasswordIncorrect", dynamic.singleton(EmailOrPasswordIncorrect))

          result(code.unauthorized, output.json(emailOrPasswordIncorrect)).toResults.to

      val post: Endpoint[Role.Guest, Librarian.Login, Either[Post, Librarian.Session]] = OtterEndpoint(
        request(method.post, url, input.json(schemas.librarian.login)),
        response(Post.results :+ result(code.created, output.json(schemas.librarian.session)))
      ).summary("Create librarian session")
        .description(
          "For security reasons, this endpoint does not give away whether the given email address is " +
            "unknown or the given password is incorrect."
        )
        .operationId("createLibrarianSession")
        .tags("librarians")
        .role(Role.Guest)
