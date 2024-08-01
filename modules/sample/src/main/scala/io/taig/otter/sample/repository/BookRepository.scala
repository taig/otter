package io.taig.otter.sample.repository

import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.taig.otter.sample.data.{Book, Isbn}
import io.taig.otter.sample.repository.BookRepository.Error

final class BookRepository(storage: AtomicCell[IO, Chain[Book]]):
  def create(create: NonEmptyChain[Book]): IO[Either[Error.Create, NonEmptyChain[Book]]] = storage
    .evalModify { books =>
      val verifyIsbns: IO[Unit] =
        val isbns = create.toList.map(_.isbn)

        (isbns diff isbns.distinct).match {
          case head :: _ => IO.raiseError(Error.Create.IsbnConflict(head))
          case Nil       => IO.unit
        } *> create.traverse_ { book =>
          IO.raiseWhen(books.exists(_.isbn === book.isbn))(Error.Create.IsbnConflict(book.isbn))
        }

      verifyIsbns.as((books ++ create.toChain, create))
    }
    .attemptNarrow[Error.Create]

  val list: IO[Chain[Book]] = storage.get

object BookRepository:
  object Error:
    enum Create extends Throwable:
      case IsbnConflict(isbn: Isbn)
