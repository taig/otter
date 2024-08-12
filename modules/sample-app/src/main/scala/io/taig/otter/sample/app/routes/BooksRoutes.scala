package io.taig.otter.sample.app.routes

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.EndpointImplementation
import io.taig.otter.sample.api.AuthenticatedRoute
import io.taig.otter.sample.api.endpoint.books.post.Error
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.app.conversion
import mouse.all.*
import io.taig.otter.sample.app.repository.BookRepository
import io.taig.otter.sample.api.schema.BookApiSchema

final class BooksRoutes(implementation: EndpointImplementation[IO], book: BookRepository):
  val post: AuthenticatedRoute[IO, BookApiSchema.Create, Either[Error, BookApiSchema]] =
    implementation(endpoint.books.post()): (_, create) =>
      book
        .create(conversion.toBookCreate(create))
        .mapIn(conversion.toBookApiSchema)
        .leftMapIn:
          case BookRepository.Error.Create.IsbnConflict => Error.IsbnConflict

object BooksRoutes:
  def apply(implementation: EndpointImplementation[IO], book: BookRepository): Routes[IO] =
    val routes = new BooksRoutes(implementation, book)
    Routes(routes.post)
