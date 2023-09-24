package io.taig.otter.sample.repository

import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import cats.effect.std.AtomicCell
import io.taig.otter.sample.Book

final class BookRepository(storage: AtomicCell[IO, Chain[Book]]):
  def create(books: NonEmptyChain[Book]): IO[NonEmptyChain[Book]] = IO.pure(books)
