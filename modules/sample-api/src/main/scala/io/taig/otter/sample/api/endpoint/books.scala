package io.taig.otter.sample.api.endpoint

import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.sample.api.schema.*
import io.taig.otter.sample.api.Role
import io.taig.otter.sample.api.AuthenticatedEndpoint

object books:
  val url: Url[Unit] = __ / "books"

  object post:
    enum Error:
      case IsbnConflict

    object Error:
      val results: Results[Error] =
        result(code.conflict, json(singleton(IsbnConflict))).toResults.to

    def apply(): AuthenticatedEndpoint[Role.Librarian, BookApiSchema.Create, Either[Error, BookApiSchema]] =
      endpoint(
        request(method.post, url, json(BookApiSchema.Create.codec)),
        response(Error.results :+ result(code.created, json(BookApiSchema.codec)))
      ).summary("Create book")
        .operationId("createBook")
        .tags("books")
        .role(Role.Librarian)
