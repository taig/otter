package io.taig.otter.sample.app

import io.taig.otter.sample.Librarian
import io.taig.otter.sample.Session
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.LibrarianApiSchema

object conversion:
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
