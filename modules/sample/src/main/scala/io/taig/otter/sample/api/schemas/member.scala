package io.taig.otter.sample.api.schemas

import io.taig.otter.Schema
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.Member

object member:
  val reference: Schema.Primitive[Member.Reference] = cistring.ivalidate(Member.Reference.validation)(_.toCIString)
  val email: Schema.Primitive[Member.Email] = cistring.ivalidate(Member.Email.validation)(_.toCIString)
  val password: Schema.Primitive[Member.Password] = string.ivalidate(Member.Password.validation)(_.toString)

  val create: Schema.Record[Member.Create] = (field("email", email) :* field("password", password)).to

  val summary: Schema.Record[Member.Summary] = (field("reference", reference) :* field("email", email)).to
