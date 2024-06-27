package io.taig.otter

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter as Base

object Playground:
  import Plain.*
  import Plain.given

  val a: Union[String] = ???

  val _: Primitive.Required.Reader[Int] = ??? // string.as(5)
