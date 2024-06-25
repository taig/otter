package io.taig.otter

import cats.syntax.all.*

object Playground:
  import Plain.*
  import Plain.given

  val x: Primitive.Required[String] = string

  val _: Primitive.Writer[String] = x.writer
  val _: Primitive.Required.Reader[String] = x.reader

  x.validate_(???)

  val y: Schema[Option[String]] = x.optional
