package io.taig.otter.http

import io.taig.otter.http.header.ContentType
import io.taig.otter.http.header.MediaRange

private[http] object Printers:
  def apply(contentType: ContentType): String =
    s"${contentType.tpe}/${contentType.subtype}" +
      contentType.parameters.map(parameter => s"; ${Printers(parameter)}").mkString

  def apply(parameter: ContentType.Parameter): String = s"${parameter.key}=\"${parameter.value}\""

  def apply(mediaRange: MediaRange.Type): String = mediaRange match
    case MediaRange.Type.Secondary(tpe, subtype) => s"$tpe/$subtype"
    case MediaRange.Type.Primary(tpe)            => s"$tpe/*"
    case MediaRange.Type.Any                     => "*/*"
