package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.{Violation, Violations}

trait Value[A] extends Schema[A]:
  override type Self[a] <: Value[a]

  def print(a: A): String | Option[String]
  def parse(value: Option[String]): Validated[Violations, A]

object Value:
  trait Required[A] extends Value[A]:
    override type Self[a] <: Value.Required[a]

    override def print(a: A): String
    final override def parse(value: Option[String]): Validated[Violations, A] =
      Validated.fromOption(value, Violations.rootNec(Violation.required)).andThen(parse)
    def parse(value: String): Validated[Violations, A]
