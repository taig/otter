package io.taig.otter.sample

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.AtomicCell
import io.taig.otter.sample.api.{Book, Librarian}
import io.taig.otter.sample.repository.{BookRepository, LibrarianRepository}
import io.taig.otter.sample.service.ReferenceGenerator

final class SampleRepositories(val books: BookRepository, val librarian: LibrarianRepository)

object SampleRepositories:
  def apply(
      references: ReferenceGenerator,
      books: AtomicCell[IO, Chain[Book]],
      librarians: AtomicCell[IO, Chain[Librarian]]
  ): SampleRepositories = new SampleRepositories(
    new BookRepository(books),
    new LibrarianRepository(references, librarians)
  )
