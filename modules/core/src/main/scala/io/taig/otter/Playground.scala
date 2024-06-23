package io.taig.otter

import cats.syntax.all.*

object Playground:
  import Plain.*
  import Plain.given

  val y: Collection[String] = ???
  val x: Schema[String] = y

  x.imap(???)(???)
  val z: Schema.Reader.Of[?, String] = x.map(???)
  // x.imap()
