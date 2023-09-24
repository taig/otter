package io.taig.otter.sample

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.AtomicCell
import io.taig.otter.sample.repository.BookRepository

final class SampleRepositories(val books: BookRepository)

object SampleRepositories:
  def apply(): IO[SampleRepositories] = AtomicCell[IO]
    .empty[Chain[Book]]
    .map(new BookRepository(_))
    .map(new SampleRepositories(_))
