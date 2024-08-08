package io.taig.otter.http.header

final case class MediaRange(tpe: MediaRange.Type, parameters: List[MediaRange.Parameter])

object MediaRange:
  enum Type:
    case Secondary(tpe: String, subtype: String)
    case Primary(tpe: String)
    case Any

  final case class Parameter(key: String, value: String)
