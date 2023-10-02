package io.taig.otter.sample.api.endpoints

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import io.taig.otter.{Codec, Union}
import io.taig.otter.dsl.*
import io.taig.otter.http.{Endpoint as OtterEndpoint, Request, Results, Url}
import io.taig.otter.sample.api.{codecs, Role}
import io.taig.otter.sample.data.{Book, Isbn}

object books:
  val url: Url[Unit] = __ / "books"

  val get: Endpoint[Role.Guest, Unit, Chain[Book]] = OtterEndpoint(
    request(method.get, url),
    response(result(code.ok, output.json(collection.chain(codecs.book.main))))
  ).tags("books").role(Role.Guest)

  enum Post:
    case IsbnConflict(isbn: Isbn)

  object Post:
    val results: Results[Post] =
      val isbnConflict: Codec[Post.IsbnConflict] = error("isbnConflict", field("isbn", codecs.isbn).to[IsbnConflict])
      result(code.conflict, output.json(isbnConflict)).to

  val post: Endpoint[Role.Librarian, NonEmptyChain[Book], Either[Post, NonEmptyChain[Book]]] =
    val books: Union[NonEmptyChain[Book]] = (codecs.book.main :+ collection.nonEmptyChain(codecs.book.main)).imap {
      case Left(book)   => NonEmptyChain.one(book)
      case Right(books) => books
    }(_.asRight)

    OtterEndpoint(
      request(method.post, url, input.json(books)).description("Lorem ipsum dolar sit amet"),
      response(Post.results :+ result(code.created, output.json(collection.nonEmptyChain(codecs.book.main))))
    ).tags("books").role(Role.Librarian)
