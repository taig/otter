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
import io.github.arainko.ducktape.*
import io.taig.otter.Data
import io.circe.JsonObject

object conversion:
  def toBookCreate(book: BookApiSchema.Create): Book.Create = book.to[Book.Create]

  given Transformer[Isbn, IsbnApiSchema] = isbn => IsbnApiSchema.unsafe(isbn.toLong)

  given Transformer[IsbnApiSchema, Isbn] = isbn => Isbn(isbn.toLong)

  given Transformer[JsonObject, Data.Object[?]] = toDataObject

  given Transformer[Data.Object[?], JsonObject] = fromData

  given Transformer[Session, SessionApiSchema] = session => SessionApiSchema(session.toUUID)

  given Transformer[SessionApiSchema, Session] = session => Session(session.toUUID)

  def toBookApiSchema(book: Book): BookApiSchema = book.to[BookApiSchema]

  def toSessionApiSchema(session: Session): SessionApiSchema = SessionApiSchema(session.toUUID)

  def toSession(session: SessionApiSchema): Session = Session(session.toUUID)

  def toLibrarianApiSchema(librarian: Librarian): LibrarianApiSchema = librarian.to[LibrarianApiSchema]

  def toLibrarianLogin(login: LibrarianApiSchema.Login): Librarian.Login = login.to[Librarian.Login]
