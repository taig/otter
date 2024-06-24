package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Validation

object Playground:
  import Plain.*
  import Plain.given

  opaque type Email = String

  // val myEmail: Validation[String, Constraint.Primitive, String, Email] = email.tap

  // val a: Primitive.Required.Reader[String] = ???
  // val b: Primitive.Required.Reader[Email] = a.validate(myEmail)

  // summon[SchemaInvariant[Schema.Of[?, *]]]
  // val b: Schema[String] = x.imap(???)(???)
  // val z: Schema.Reader.Of[?, String] = x.map(???)
  // val a: Schema.Writer[?] = x.contramap(???)

  // val v: Primitive.Required.Reader[String] = string.map(???)
