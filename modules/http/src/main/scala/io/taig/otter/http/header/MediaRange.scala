package io.taig.otter.http.header

import cats.Show
import io.taig.otter.http.Printers

final case class MediaRange(tpe: MediaRange.Type, parameters: List[MediaRange.Parameter])

object MediaRange:
  enum Type:
    case Secondary(tpe: String, subtype: String)
    case Primary(tpe: String)
    case Any

  object Type:
    given Show[MediaRange.Type] = Printers(_)

  final case class Parameter(key: String, value: String)

  // object Parameter:
  //   given Show[MediaRange.Parameter] = Printers(_)
