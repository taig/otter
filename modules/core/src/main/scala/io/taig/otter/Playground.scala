package io.taig.otter

import io.taig.otter as Base
import cats.syntax.all.*
import cats.Id
import cats.Comonad

final case class Annotation[+A](metadata: Unit, self: A)

object Annotation:
  given ApplicativeComonad[Annotation] = ???

object Playground:
  import Plain.*

  val x: Base.Schema.Reader[Annotation, ?, ?, String] = ???
