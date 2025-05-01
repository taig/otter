// package io.taig.otter.sample.app.repository

// import cats.data.Chain
// import cats.effect.IO
// import cats.effect.std.AtomicCell
// import cats.syntax.all.*
// import io.taig.otter.sample.Book
// import io.taig.otter.sample.app.repository.BookRepository.Error

// final class BookRepository(storage: AtomicCell[IO, Chain[Book]]):
//   def create(book: Book.Create): IO[Either[Error.Create, Book]] = storage
//     .evalModify: books =>
//       IO.raiseWhen(books.exists(_.isbn === book.isbn))(Error.Create.IsbnConflict)
//         .as((books :+ book.toBook, book.toBook))
//     .attemptNarrow[Error.Create]

//   val list: IO[Chain[Book]] = storage.get

// object BookRepository:
//   object Error:
//     enum Create extends Throwable:
//       case IsbnConflict
