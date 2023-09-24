package io.taig.otter.sample.routes

import cats.data.NonEmptyChain
import cats.effect.IO
import io.taig.otter.http.{Route, Routes}
import io.taig.otter.sample.repository.BookRepository
import io.taig.otter.sample.util.EndpointImplementation
import io.taig.otter.sample.{endpoints, Book}

final class BooksRoutes(implementation: EndpointImplementation, books: BookRepository):
  def post: Route[IO, NonEmptyChain[Book], NonEmptyChain[Book]] =
    implementation(endpoints.books.post)(this.books.create)

object BooksRoutes:
  def apply(implementation: EndpointImplementation, books: BookRepository): Routes[IO] =
    val routes = new BooksRoutes(implementation, books)
    Routes(routes.post)
