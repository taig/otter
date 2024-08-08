package io.taig.otter.http

import io.taig.otter.http.header.ContentType
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.Parameter
import cats.Show
import cats.syntax.all.*
import io.taig.otter.http.header.Weighted

private[http] object Printers:
  def apply(contentType: ContentType): String =
    s"${contentType.tpe}/${contentType.subtype}" +
      contentType.parameters.map(parameter => s"; ${Printers(parameter)}").mkString

  def apply(parameter: Parameter): String = s"${parameter.name}=\"${parameter.value}\""

  def apply(parameters: List[Parameter]): String = parameters.map(parameter => s"; ${Printers(parameter)}").mkString

  def apply(mediaRange: MediaRange.Type): String = mediaRange match
    case MediaRange.Type.Secondary(tpe, subtype) => s"$tpe/$subtype"
    case MediaRange.Type.Primary(tpe)            => s"$tpe/*"
    case MediaRange.Type.Any                     => "*/*"

  def apply(mediaRange: MediaRange): String =
    s"${Printers(mediaRange.tpe)}${Printers(mediaRange.parameters)}"

  def apply[A: Show](weighted: Weighted[A]): String = weighted match
    case Weighted(self, None)         => self.show
    case Weighted(self, Some(weight)) => show"$self; q=$weight"

  def apply(formData: FormData): String = formData.toVector
      .map:
        case (key, Some(value)) => s"$key=$value"
        case (key, None) => key
      .mkString("&")
