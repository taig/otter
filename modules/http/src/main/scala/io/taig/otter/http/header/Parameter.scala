package io.taig.otter.http.header

import cats.Eq
import cats.Show
import cats.parse.Parser
import io.taig.otter.http.Parsers
import cats.syntax.all.*
import org.typelevel.ci.CIString

final case class Parameter(name: CIString, value: String):
  override def toString: String = show"$name=\"$value\""

object Parameter:
  def parse(value: String): Either[Parser.Error, Parameter] =
    Parsers.parameter.parseAll(value)

  given Eq[Parameter] = Eq.by(parameter => (parameter.name, parameter.value))

  given Show[Parameter] = Show.fromToString
