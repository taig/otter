package io.taig.otter.sample.app.routes

import cats.effect.IO
import io.github.arainko.ducktape.*
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.EndpointImplementation
import io.taig.otter.sample.api.AuthenticatedRoute
import io.taig.otter.sample.api.endpoint.books.post.Error
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.app.transformers.given
import mouse.all.*
import io.taig.otter.sample.app.repository.BookRepository
import io.taig.otter.sample.api.schema.BookApiSchema
import io.taig.otter.sample.Book
import cats.implicits.*

final class BooksRoutes(implementation: EndpointImplementation[IO], book: BookRepository):
  val get: AuthenticatedRoute[IO, Unit, List[BookApiSchema.Summary]] = 
    implementation(endpoint.books.get()): (_, _) =>
      book.list.map(_.toList.map(_.to[BookApiSchema.Summary]))

  val post: AuthenticatedRoute[IO, BookApiSchema.Create, Either[Error, BookApiSchema]] =
    implementation(endpoint.books.post()): (_, create) =>
      book
        .create(create.to[Book.Create])
        .mapIn(_.to[BookApiSchema])
        .leftMapIn:
          case BookRepository.Error.Create.IsbnConflict => Error.IsbnConflict

object BooksRoutes:
  def apply(implementation: EndpointImplementation[IO], book: BookRepository): Routes[IO] =
    val routes = new BooksRoutes(implementation, book)
    Routes(routes.get, routes.post)
