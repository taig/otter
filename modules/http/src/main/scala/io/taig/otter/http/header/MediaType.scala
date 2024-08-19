package io.taig.otter.http.header

import io.taig.otter.http.Dsl.*
import io.taig.otter.http.Printers
import io.taig.otter.http.Parsers
import cats.Show
import cats.syntax.all.*
import cats.parse.Parser
import cats.Eq

final case class MediaType(tpe: MediaType.Type, parameters: Parameters):
  // TODO respect parameters (?)
  def satisfies(mediaRange: MediaRange): Boolean = mediaRange.tpe match
    case MediaRange.Type.Secondary(primary, secondary) => primary === tpe.primary && secondary === tpe.secondary
    case MediaRange.Type.Primary(primary) => primary === tpe.primary
    case MediaRange.Type.Any => true

  override def toString: String = Printers(this)

object MediaType:
  final case class Type(primary: String, secondary: String):
    override def toString: String = Printers(this)

  object Type:
    given Eq[MediaType.Type] = Eq.by(tpe => (tpe.primary, tpe.secondary))

    given Show[MediaType.Type] = Show.fromToString

  def parse(value: String): Either[Parser.Error, MediaType] = Parsers.mediaType.parseAll(value)

  val codec: Primitive.Required[MediaType] = parser(name = "contentType")(parse(_).toOption)(_.show)

  given Eq[MediaType] = Eq.by(mediaType => (mediaType.tpe, mediaType.parameters))

  given Show[MediaType] = Show.fromToString
