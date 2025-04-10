package io.taig.otter.http.header

import cats.syntax.all.*
import cats.Show

final case class MediaRange(tpe: MediaRange.Type, parameters: Parameters):
  override def toString: String = show"$tpe$parameters"

object MediaRange:
  enum Type:
    case Secondary(primary: String, secondary: String)
    case Primary(primary: String)
    case Any

    override def toString: String = this match
      case MediaRange.Type.Secondary(tpe, subtype) => s"$tpe/$subtype"
      case MediaRange.Type.Primary(tpe)            => s"$tpe/*"
      case MediaRange.Type.Any                     => "*/*"

  object Type:
    given Show[MediaRange.Type] = Show.fromToString

  val Any: MediaRange = MediaRange(Type.Any, Parameters.Empty)

  given Show[MediaRange] = Show.fromToString
