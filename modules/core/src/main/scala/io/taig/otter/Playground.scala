package io.taig.otter

import cats.syntax.all.*

object Playground:
  import Plain.*
  import Plain.given

  val x: Primitive.Required[String] = string

  val y: Schema[Option[String]] = x.optional
