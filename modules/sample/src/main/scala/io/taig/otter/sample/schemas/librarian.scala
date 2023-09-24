package io.taig.otter.sample.schemas

import io.taig.otter.Schema
import io.taig.otter.sample.Librarian
import io.taig.otter.dsl.*

object librarian:
  val reference: Schema.Primitive[Librarian.Reference] =
    cistring.ivalidate(Librarian.Reference.validation)(_.toCIString)
  val email: Schema.Primitive[Librarian.Email] =
    cistring.ivalidate(Librarian.Email.validation)(_.toCIString)

  val main: Schema.Record[Librarian] = (field("reference", reference) :* field("email", email)).to
