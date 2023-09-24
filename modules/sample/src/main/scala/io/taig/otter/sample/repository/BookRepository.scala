package io.taig.otter.sample.repository

import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.taig.otter.sample.repository.BookRepository.Error
import io.taig.otter.sample.{Book, Isbn}

import scala.util.control.NoStackTrace

final class BookRepository(storage: AtomicCell[IO, Chain[Book]]):
  def create(books: NonEmptyChain[Book]): IO[Either[Error.Create, NonEmptyChain[Book]]] =
    def verifyIsbns(current: Chain[Book]): IO[Unit] = books.traverse_ { book =>
      IO.raiseWhen(current.exists(_.isbn === book.isbn))(Error.Create.IsbnConflict(book.isbn))
    }

    storage
      .evalUpdate(current => verifyIsbns(current) *> IO.pure(current ++ books.toChain))
      .as(books)
      .attemptNarrow[Error.Create]

object BookRepository:
  object Error:
    enum Create extends NoStackTrace:
      case IsbnConflict(isbn: Isbn)
