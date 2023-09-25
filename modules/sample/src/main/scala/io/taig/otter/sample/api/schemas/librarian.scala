package io.taig.otter.sample.api.schemas

import io.taig.otter.Schema
import io.taig.otter.sample.Librarian
import io.taig.otter.dsl.*

object librarian:
  val reference: Schema.Primitive[Librarian.Reference] =
    cistring.ivalidate(Librarian.Reference.validation)(_.toCIString)
  val email: Schema.Primitive[Librarian.Email] = cistring.ivalidate(Librarian.Email.validation)(_.toCIString)
  val password: Schema.Primitive[Librarian.Password] = string.ivalidate(Librarian.Password.validation)(_.toString)
  val session: Schema.Primitive[Librarian.Session] = uuid.imap(Librarian.Session.fromUUID)(_.toUUID)

  val summary: Schema.Record[Librarian.Summary] = (
    field("reference", reference) :*
      field("email", email)
  ).to
