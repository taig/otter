package io.taig.otter.sample.app

import io.taig.otter.sample.app.repository.LibrarianRepository
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.data.Chain
import io.taig.otter.sample.Librarian

final class SampleRepositories(librarians: AtomicCell[IO, Chain[Librarian]]):
  val librarian: LibrarianRepository = new LibrarianRepository(librarians)

object SampleRepositories:
  def apply(): IO[SampleRepositories] = (
    AtomicCell[IO].empty[Chain[Librarian]],
  ).map(new SampleRepositories(_))
