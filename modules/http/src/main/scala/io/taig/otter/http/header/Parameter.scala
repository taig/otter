package io.taig.otter.http.header

import io.taig.otter.http.Printers
import io.taig.otter.http.Parsers
import cats.parse.Parser

final case class Parameter(key: String, value: String):
  override def toString: String = Printers(this)

object Parameter:
  def parse(value: String): Either[Parser.Error, Parameter] =
    Parsers.parameter.parseAll(value)
