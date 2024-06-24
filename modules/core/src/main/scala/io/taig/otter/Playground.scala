package io.taig.otter

// import cats.syntax.all.*

object Playground:
  import Plain.*
  import Plain.given

  val y: Collection[String] = ???
  val x: Schema[String] = y

  // summon[SchemaInvariant[Schema.Of[?, *]]]
  // val b: Schema[String] = x.imap(???)(???)
  // val z: Schema.Reader.Of[?, String] = x.map(???)
  // val a: Schema.Writer[?] = x.contramap(???)

  // val v: Primitive.Required.Reader[String] = string.map(???)
