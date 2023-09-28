package io.taig.otter.sample.api.schemas

import io.taig.otter.Schema
import io.taig.otter.Primitive
import io.taig.otter.Record
import io.taig.otter.sample.api.Librarian
import io.taig.otter.dsl.*

object librarian:
  val reference: Primitive[Librarian.Reference] =
    cistring.ivalidate(Librarian.Reference.validation)(_.toCIString)
  val email: Primitive[Librarian.Email] = cistring.ivalidate(Librarian.Email.validation)(_.toCIString)
  val password: Primitive[Librarian.Password] = string.ivalidate(Librarian.Password.validation)(_.toString)
  val session: Primitive[Librarian.Session] = uuid.imap(Librarian.Session.fromUUID)(_.toUUID)

  val login: Record[Librarian.Login] = (field("email", cistring) :* field("password", string)).to

  val summary: Record[Librarian.Summary] = (
    field("reference", reference) :*
      field("email", email)
  ).to
