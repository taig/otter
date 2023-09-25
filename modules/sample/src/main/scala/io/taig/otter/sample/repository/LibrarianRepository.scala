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
  def create(librarian: Librarian.Create): IO[Either[Error.Create, Librarian.Summary]] =
    storage
      .evalModify { librarians =>
        val verifyEmail: IO[Unit] =
          IO.raiseWhen(librarians.exists(_.email === librarian.email))(Error.Create.EmailConflict)

        val generateReference: IO[Librarian.Reference] =
          references.generate(Librarian.Reference.Length).map(Librarian.Reference.unsafeFromCIString)

        for
          _ <- verifyEmail
          reference <- generateReference
          value = Librarian(reference, librarian.email, librarian.password, session = none)
        yield (librarians :+ value, value.toSummary)
      }
      .attemptNarrow[Error.Create]

object LibrarianRepository:
  object Error:
    enum Create extends NoStackTrace:
      case EmailConflict
