package io.taig.otter.http.header

import io.taig.otter.http.Printers
import io.taig.otter.http.Parsers
import cats.parse.Parser

final case class ContentType(tpe: String, subtype: String, parameters: List[ContentType.Parameter]):
  def print: String = Printers(this)
  override def toString: String = print

object ContentType:
  final case class Parameter(key: String, value: String):
    def print: String = Printers(this)
    override def toString: String = print

  object Parameter:
    def parse(value: String): Either[Parser.Error, ContentType.Parameter] =
      Parsers.contentType.parameter.parseAll(value)

  def parse(value: String): Either[Parser.Error, ContentType] = Parsers.contentType.root.parseAll(value)
