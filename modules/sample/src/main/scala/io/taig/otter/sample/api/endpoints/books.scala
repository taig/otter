package io.taig.otter.sample.api.endpoints

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import io.taig.otter.dsl.*
import io.taig.otter.http.{Request, Results, Url}
import io.taig.otter.sample.api.{schemas, Role}
import io.taig.otter.sample.data.{Book, Isbn}
import io.taig.otter.Schema

object books:
  val url: Url[Unit] = __ / "books"

  val get: Endpoint[Role.Guest, Unit, Chain[Book]] = Endpoint(
    request(method.get, url),
    response(result(code.ok, output.json(collection.chain(schemas.book.main))))
  ).tags("books")

  enum Post:
    case IsbnConflict(isbn: Isbn)

  object Post:
    val results: Results[Post] =
      val isbnConflict: Schema[Post.IsbnConflict] = error("isbnConflict", field("isbn", schemas.isbn).to[IsbnConflict])
      result(code.conflict, output.json(isbnConflict)).to

  val post: Endpoint[Role.Librarian, NonEmptyChain[Book], Either[Post, NonEmptyChain[Book]]] =
    val books: Schema[NonEmptyChain[Book]] = schemas.book.main
      .orElse(collection.nonEmptyChain(schemas.book.main))
      .imap {
        case Left(book)   => NonEmptyChain.one(book)
        case Right(books) => books
      }(_.asRight)

    Endpoint(
      request(method.post, url, input.json(books)).description("Lorem ipsum dolar sit amet"),
      response(Post.results :+ result(code.created, output.json(collection.nonEmptyChain(schemas.book.main))))
    ).tags("books")
