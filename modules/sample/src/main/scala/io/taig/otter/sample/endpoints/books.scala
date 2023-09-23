package io.taig.otter.sample.endpoints

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.{Discriminator, Schema}
import io.taig.otter.http.{Endpoint, Request, Url}
import io.taig.otter.dsl.*
import io.taig.otter.sample.Book
import io.taig.otter.sample.schemas

object books:
  val root: Url[Unit] = __ / "books"

  val post: Endpoint[NonEmptyChain[Book], NonEmptyChain[Book]] =
    val books: Schema.Coproduct[NonEmptyChain[Book]] = (
      branch("book", schemas.book.main) :+
        branch("books", collection.nonEmptyChain(schemas.book.main))
    ).discriminator(Discriminator.None)
      .imap {
        case Left(book)   => NonEmptyChain.one(book)
        case Right(books) => books
      }(_.asRight)

    Endpoint(
      request(method.post, root, input.json(books)),
      response(result(code.created, output.json(collection.nonEmptyChain(schemas.book.main))))
    )
