package io.taig.otter.sample.app

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.taig.otter.sample.Book
import io.taig.otter.sample.Librarian
import io.taig.otter.sample.app.repository.BookRepository
import io.taig.otter.sample.app.repository.LibrarianRepository

final class SampleRepositories(books: AtomicCell[IO, Chain[Book]], librarians: AtomicCell[IO, Chain[Librarian]]):
  val book: BookRepository = new BookRepository(books)
  val librarian: LibrarianRepository = new LibrarianRepository(librarians)

object SampleRepositories:
  def apply(): IO[SampleRepositories] = (
    AtomicCell[IO].empty[Chain[Book]],
    AtomicCell[IO].empty[Chain[Librarian]]
  ).mapN(new SampleRepositories(_, _))
