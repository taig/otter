// package io.taig.otter.sample.repository

// import cats.data.Chain
// import cats.effect.IO
// import cats.effect.std.{AtomicCell, UUIDGen}
// import cats.syntax.all.*
// import io.taig.otter.sample.data.{Librarian, Session}
// import io.taig.otter.sample.repository.LibrarianRepository.Error
// import io.taig.otter.sample.service.ReferenceGenerator

// import scala.util.control.NoStackTrace

// final class LibrarianRepository(references: ReferenceGenerator, storage: AtomicCell[IO, Chain[Librarian]]):
//   def create(librarian: Librarian.Create): IO[Either[Error.Create, Librarian.Summary]] =
//     storage
//       .evalModify { librarians =>
//         val verifyEmail: IO[Unit] =
//           IO.raiseWhen(librarians.exists(_.email === librarian.email))(Error.Create.EmailConflict)

//         val generateReference: IO[Librarian.Reference] =
//           references.generate(Librarian.Reference.Length).map(Librarian.Reference.unsafeFromCIString)

//         for
//           _ <- verifyEmail
//           reference <- generateReference
//           value = Librarian(reference, librarian.email, librarian.password, session = none)
//         yield (librarians :+ value, value.toSummary)
//       }
//       .attemptNarrow[Error.Create]

//   def findBySession(session: Session): IO[Option[Librarian.Summary]] =
//     storage.get.map(_.find(_.session.contains_(session)).map(_.toSummary))

//   def login(librarian: Librarian.Login): IO[Either[Error.Login, Session]] = storage
//     .evalModify { librarians =>
//       for
//         self <- librarians.find(_.email.toCIString === librarian.email).liftTo[IO](Error.Login.EmailUnknown)
//         _ <- IO.raiseWhen(self.password.toString =!= librarian.password)(Error.Login.PasswordIncorrect)
//         session <- UUIDGen.randomUUID[IO].map(Session.fromUUID)
//         update = librarians.map:
//           case librarian if librarian.email === self.email => librarian.copy(session = session.some)
//           case librarian                                   => librarian
//       yield (update, session)
//     }
//     .attemptNarrow[Error.Login]

// object LibrarianRepository:
//   object Error:
//     enum Create extends NoStackTrace:
//       case EmailConflict

//     enum Login extends NoStackTrace:
//       case EmailUnknown
//       case PasswordIncorrect
