package io.taig.otter.sample.app

import io.taig.otter.sample.app.repository.LibrarianRepository
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.std.AtomicCell
import cats.data.Chain
import io.taig.otter.sample.Librarian
import io.taig.otter.sample.app.repository.BookRepository
import io.taig.otter.sample.Book

final class SampleRepositories(books: AtomicCell[IO, Chain[Book]], librarians: AtomicCell[IO, Chain[Librarian]]):
  val book: BookRepository = new BookRepository(books)
  val librarian: LibrarianRepository = new LibrarianRepository(librarians)

object SampleRepositories:
  def apply(): IO[SampleRepositories] = (
    AtomicCell[IO].empty[Chain[Book]],
    AtomicCell[IO].empty[Chain[Librarian]]
  ).mapN(new SampleRepositories(_, _))
