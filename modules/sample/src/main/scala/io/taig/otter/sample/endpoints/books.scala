package io.taig.otter.sample.endpoints

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.{Discriminator, Schema}
import io.taig.otter.http.{Endpoint, Request, Results, Url}
import io.taig.otter.dsl.*
import io.taig.otter.sample.Book
import io.taig.otter.sample.Isbn
import io.taig.otter.sample.schemas

object books:
  val root: Url[Unit] = __ / "books"

  enum Post:
    case IsbnConflict(isbn: Isbn)
    case Foobar

  object Post:
    val results: Results[Post] =
      val isbnConflict: Schema[Post.IsbnConflict] = error("isbnConflict", field("isbn", schemas.isbn).to[IsbnConflict])
      val foobar: Schema[Foobar.type] = error("foobar", dynamic.singleton(Foobar))

      (result(code.conflict, output.json(isbnConflict)) :+ result(code.badRequest, output.json(foobar))).to

  val post: Endpoint[NonEmptyChain[Book], Either[Post, NonEmptyChain[Book]]] =
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
      response(Post.results :+ result(code.created, output.json(collection.nonEmptyChain(schemas.book.main))))
    )
