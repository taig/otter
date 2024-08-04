package io.taig.otter.sample.api.endpoints

import io.taig.otter.sample.Dsl.*

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

//       val post: AuthenticatedEndpoint[Role.Guest, Librarian.Login, Either[Post, Session]] = endpoint(
//         request(method.post, url, input.json(codecs.librarian.login)),
//         response(Post.results :+ result(code.created, output.json(codecs.session)))
//       ).summary("Create librarian session")
//         .description(
//           "For security reasons, this endpoint does not give away whether the given email address is " +
//             "unknown or the given password is incorrect."
//         )
//         .operationId("createLibrarianSession")
//         .tags("librarians")
//         .role(Role.Guest)
