package io.taig.otter

import io.taig.otter as Base
import cats.Id

object Playground:
  import Plain.*

  val y: Base.Schema[Id, String, ?, String] = ???
  val x: Schema[String] = y
