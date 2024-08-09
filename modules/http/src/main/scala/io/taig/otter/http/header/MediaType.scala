package io.taig.otter.http.header

import io.taig.otter.http.Dsl.*
import io.taig.otter.http.Printers
import io.taig.otter.http.Parsers
import cats.Show
import cats.syntax.all.*
import cats.parse.Parser

final case class MediaType(tpe: MediaType.Type, parameters: List[Parameter]):
  override def toString: String = Printers(this)

object MediaType:
  final case class Type(primary: String, secondary: String):
    override def toString: String = Printers(this)

  object Type:
    given Show[MediaType.Type] = Show.fromToString

  def parse(value: String): Either[Parser.Error, MediaType] = Parsers.mediaType.parseAll(value)

  val codec: Primitive.Required[MediaType] = parser(name = "contentType")(parse(_).toOption)(_.show)

  given Show[MediaType] = Show.fromToString