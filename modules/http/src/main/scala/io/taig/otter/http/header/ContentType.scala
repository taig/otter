package io.taig.otter.http.header

import io.taig.otter.http.Printers
import io.taig.otter.http.Parsers
import io.taig.otter.http.Dsl.*
import cats.parse.Parser
import cats.Show
import cats.syntax.all.*

final case class ContentType(tpe: String, subtype: String, parameters: List[Parameter]):
  override def toString: String = Printers(this)

object ContentType:
  def parse(value: String): Either[Parser.Error, ContentType] = Parsers.contentType.parseAll(value)

  val codec: Primitive.Required[ContentType] = parser(name = "contentType")(parse(_).toOption)(_.show)

  given Show[ContentType] = Show.fromToString
