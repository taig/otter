package io.taig.otter.sample.api.schemas

import io.taig.otter.{Primitive, Record}
import io.taig.otter.dsl.*
import io.taig.otter.sample.data.Member

object member:
  val reference: Primitive[Member.Reference] = cistring.ivalidate(Member.Reference.validation)(_.toCIString)
  val email: Primitive[Member.Email] = cistring.ivalidate(Member.Email.validation)(_.toCIString)
  val password: Primitive[Member.Password] = string.ivalidate(Member.Password.validation)(_.toString)

  val create: Record[Member.Create] = (field("email", email) :* field("password", password)).to

  val summary: Record[Member.Summary] = (field("reference", reference) :* field("email", email)).to
