package io.taig.otter.sample.api.codecs

import io.taig.otter.{Primitive, Record}
import io.taig.otter.dsl.*
import io.taig.otter.sample.data.Librarian

object librarian:
  val reference: Primitive[Librarian.Reference] =
    cistring.ivalidate(Librarian.Reference.validation)(_.toCIString)
  val email: Primitive[Librarian.Email] = cistring.ivalidate(Librarian.Email.validation)(_.toCIString)
  val password: Primitive[Librarian.Password] = string.ivalidate(Librarian.Password.validation)(_.toString)

  val login: Record[Librarian.Login] = (field("email", cistring) :* field("password", string)).to

  val summary: Record[Librarian.Summary] = (
    field("reference", reference) :*
      field("email", email)
  ).to
