package io.taig.otter.sample.endpoints

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.http.{Endpoint, Request, Url}
import io.taig.otter.dsl.*
import io.taig.otter.sample.Book
import io.taig.otter.sample.schemas
import io.taig.otter.schema.Coproduct

object books:
  val root: Url[Unit] = __ / "books"

  val post: Endpoint[NonEmptyChain[Book], NonEmptyChain[Book]] =
    val books: Coproduct[NonEmptyChain[Book]] = (
      branch("book", schemas.book.main) :+
        branch("books", collection.nonEmptyChain(schemas.book.main))
    ).discriminator.none
      .imap {
        case Left(book)   => NonEmptyChain.one(book)
        case Right(books) => books
      }(_.asRight)

    Endpoint(
      request(method.post, root, request.of(???, books)),
      response(result(code.created, response.of(???, collection.nonEmptyChain(schemas.book.main))))
    )
