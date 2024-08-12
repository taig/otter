package io.taig.otter.sample.app

import io.taig.otter.sample.Librarian
import io.taig.otter.sample.Session
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.api.schema.BookApiSchema
import io.taig.otter.sample.Book
import io.taig.otter.sample.api.schema.IsbnApiSchema
import cats.implicits.*
import io.taig.otter.json.*
import io.taig.otter.sample.api.schema.BookApiSchema.Genre
import io.taig.otter.sample.Isbn

object conversion:
  def toBookCreate(book: BookApiSchema.Create): Book.Create = Book.Create(
    isbn = Isbn(book.isbn.toLong),
    title = book.title,
    genres = book.genres.map(toBookGenre),
    metadata = fromData(book.metadata)
  )

  def toBookApiSchema(book: Book): BookApiSchema = BookApiSchema(
    isbn = IsbnApiSchema.unsafe(book.isbn.toLong),
    title = book.title,
    genres = book.genres.map(toBookApiSchemaGenre),
    metadata = toDataObject(book.metadata)
  )

  def toBookApiSchemaGenre(genre: Book.Genre): BookApiSchema.Genre = genre match
    case Book.Genre.Biography => BookApiSchema.Genre.Biography
    case Book.Genre.Children  => BookApiSchema.Genre.Children
    case Book.Genre.Fantasy   => BookApiSchema.Genre.Fantasy
    case Book.Genre.Poetry    => BookApiSchema.Genre.Poetry
    case Book.Genre.Romance   => BookApiSchema.Genre.Romance
    case Book.Genre.Thriller  => BookApiSchema.Genre.Thriller

  def toBookGenre(genre: BookApiSchema.Genre): Book.Genre = genre match
    case BookApiSchema.Genre.Biography => Book.Genre.Biography
    case BookApiSchema.Genre.Children  => Book.Genre.Children
    case BookApiSchema.Genre.Fantasy   => Book.Genre.Fantasy
    case BookApiSchema.Genre.Poetry    => Book.Genre.Poetry
    case BookApiSchema.Genre.Romance   => Book.Genre.Romance
    case BookApiSchema.Genre.Thriller  => Book.Genre.Thriller

  def toSessionApiSchema(session: Session): SessionApiSchema = SessionApiSchema(session.toUUID)

  def toSession(session: SessionApiSchema): Session = Session(session.toUUID)

  def toLibrarianApiSchema(librarian: Librarian): LibrarianApiSchema = LibrarianApiSchema(
    reference = librarian.reference,
    email = librarian.email,
    password = librarian.password,
    session = librarian.session.map(toSessionApiSchema)
  )

  def toLibrarianLogin(login: LibrarianApiSchema.Login): Librarian.Login = Librarian.Login(
    email = login.email,
    password = login.password
  )
