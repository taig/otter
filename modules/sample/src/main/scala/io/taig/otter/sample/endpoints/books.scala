package io.taig.otter.sample.endpoints

import io.taig.otter.http.{Endpoint, Request, Url}
import io.taig.otter.dsl.*
import io.taig.otter.sample.Book
import io.taig.otter.sample.schemas

object books:
  val root: Url[Unit] = __ / "books"

  val post: Endpoint[Book, Book] = Endpoint(
    request(method.post, root, request.of(???, schemas.book.main)),
    ???
  )
