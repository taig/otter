package io.taig.otter.http.header

import cats.Eq
import cats.Show
import cats.parse.Parser
import cats.syntax.all.*
import io.taig.otter.http.Parsers

final case class MediaType(tpe: MediaType.Type, parameters: Parameters):
  // TODO respect parameters (?)
  def satisfies(mediaRange: MediaRange): Boolean = tpe.satisfies(mediaRange = mediaRange.tpe)

  override def toString: String = tpe.show + parameters.show

object MediaType:
  final case class Type(primary: String, secondary: String):
    def satisfies(mediaRange: MediaRange.Type): Boolean = mediaRange match
      case reference: MediaRange.Type.Secondary => primary === reference.primary && secondary === reference.secondary
      case reference: MediaRange.Type.Primary   => primary === reference.primary
      case MediaRange.Type.Any                  => true

    override def toString: String = show"${primary}/${secondary}"

  object Type:
    given Eq[MediaType.Type] = Eq.by(tpe => (tpe.primary, tpe.secondary))

    given Show[MediaType.Type] = Show.fromToString

  def parse(value: String): Either[Parser.Error, MediaType] = Parsers.mediaType.parseAll(value)

  // val codec: Primitive[MediaType] = parser(name = "contentType")(parse(_).toOption)(_.show)

  given Eq[MediaType] = Eq.by(mediaType => (mediaType.tpe, mediaType.parameters))

  given Show[MediaType] = Show.fromToString
