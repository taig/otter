package io.taig.otter.http.header

import io.taig.otter.http.Printers
import io.taig.otter.http.Parsers
import cats.parse.Parser
import org.typelevel.ci.CIString

final case class Parameter(name: CIString, value: String):
  override def toString: String = Printers(this)

object Parameter:
  def parse(value: String): Either[Parser.Error, Parameter] =
    Parsers.parameter.parseAll(value)
