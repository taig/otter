package io.taig.otter

import cats.syntax.all.*

object Playground:
  import Plain.*
  import Plain.given

  val x: Primitive[String] = string

  string.as(3)
  val z: Primitive.Reader[Int] = x.as(3)
