package io.taig.otter.sample.repository

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import io.taig.otter.sample.Librarian
import io.taig.otter.sample.repository.LibrarianRepository.Error
import io.taig.otter.sample.service.ReferenceGenerator

import scala.util.control.NoStackTrace

final class LibrarianRepository(references: ReferenceGenerator, storage: AtomicCell[IO, Chain[Librarian]]):
  def create(librarian: Librarian.Create): IO[Either[Error.Create, Librarian]] = (for
    reference <- references.generate(Librarian.Reference.Length).map(Librarian.Reference.unsafeFromCIString)
    result = Librarian(reference, librarian.email)
    _ <- storage.update(result +: _)
  yield result).attemptNarrow[Error.Create]

object LibrarianRepository:
  object Error:
    enum Create extends NoStackTrace:
      case EmailConflict
