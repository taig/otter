package io.taig.otter.sample

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.taig.otter.sample.api.{Book, Librarian}
import io.taig.otter.sample.repository.{BookRepository, LibrarianRepository}
import io.taig.otter.sample.service.ReferenceGenerator

final class SampleRepositories(val books: BookRepository, val librarian: LibrarianRepository)

object SampleRepositories:
  def apply(references: ReferenceGenerator): IO[SampleRepositories] = (
    AtomicCell[IO].empty[Chain[Book]].map(new BookRepository(_)),
    AtomicCell[IO].empty[Chain[Librarian]].map(new LibrarianRepository(references, _))
  ).mapN(new SampleRepositories(_, _))
