package io.taig.otter.sample.schemas

import io.taig.otter.Schema
import io.taig.otter.sample.Administrator
import io.taig.otter.dsl.*

object administrator:
  val reference: Schema.Primitive[Administrator.Reference] =
    cistring.ivalidate(Administrator.Reference.validation)(_.toCIString)
  val email: Schema.Primitive[Administrator.Email] =
    cistring.ivalidate(Administrator.Email.validation)(_.toCIString)

  val main: Schema.Record[Administrator] = (field("reference", reference) :* field("email", email)).to
