package io.taig.otter.sample.routes

import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.api.Route
import io.taig.otter.sample.api.endpoints.books.Post
import io.taig.otter.sample.repository.BookRepository
import io.taig.otter.sample.repository.BookRepository.Error
import io.taig.otter.sample.{Book, Librarian, Member, SampleRoute}
import mouse.all.*

final class BooksRoutes(route: SampleRoute, books: BookRepository):
  val get: Route[Unit, Chain[Book]] = route(endpoints.books.get)((_, _) => books.list)

  val post: Route[NonEmptyChain[Book], Either[Post, NonEmptyChain[Book]]] =
    route(endpoints.books.post): (self, books) =>
      this.books
        .create(books)
        .leftMapIn:
          case Error.Create.IsbnConflict(isbn) => Post.IsbnConflict(isbn)

object BooksRoutes:
  def apply(route: SampleRoute, books: BookRepository): Routes[IO] =
    val routes = new BooksRoutes(route, books)
    Routes(routes.get, routes.post)
