package io.taig.otter

import cats.syntax.all.*
import cats.Functor
import cats.data.Func

object Playground:
  import Plain.*

  // string.imap(???)(???)
  // string.contramap(???)
  // string.map(???)

  val x: Schema[String] = ???
  x.imap(_.reverse)(_.reverse)
  x.map(???)
