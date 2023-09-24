package io.taig.otter.sample.routes

import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import io.taig.otter.http.{Route, Routes}
import io.taig.otter.sample.endpoints.books.Post
import io.taig.otter.sample.repository.BookRepository
import io.taig.otter.sample.repository.BookRepository.Error
import io.taig.otter.sample.util.EndpointImplementation
import io.taig.otter.sample.{endpoints, Book}
import mouse.all.*

final class BooksRoutes(implementation: EndpointImplementation, books: BookRepository):
  val get: Route[IO, Unit, Chain[Book]] = implementation(endpoints.books.get)(_ => books.list)

  val post: Route[IO, NonEmptyChain[Book], Either[Post, NonEmptyChain[Book]]] =
    implementation(endpoints.books.post): books =>
      this.books
        .create(books)
        .leftMapIn:
          case Error.Create.IsbnConflict(isbn) => Post.IsbnConflict(isbn)

object BooksRoutes:
  def apply(implementation: EndpointImplementation, books: BookRepository): Routes[IO] =
    val routes = new BooksRoutes(implementation, books)
    Routes(routes.get, routes.post)
