package io.taig.otter.http.header

import cats.Show
import io.taig.otter.http.Printers

final case class MediaRange(tpe: MediaRange.Type, parameters: Parameters):
  override def toString: String = Printers(this)

object MediaRange:
  enum Type:
    case Secondary(tpe: String, subtype: String)
    case Primary(tpe: String)
    case Any

    override def toString: String = Printers(this)

  object Type:
    given Show[MediaRange.Type] = Show.fromToString

  given Show[MediaRange] = Show.fromToString
