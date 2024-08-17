package io.taig.otter.http

import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameter
import cats.Show
import cats.syntax.all.*
import io.taig.otter.http.header.Weighted
import io.taig.otter.http.header.Parameters

private[http] object Printers:
  def apply(parameter: Parameter): String = s"${parameter.name}=\"${parameter.value}\""

  def apply(parameters: Parameters): String = parameters.toList.map(parameter => s"; ${Printers(parameter)}").mkString

  def apply(mediaType: MediaType.Type): String = s"${mediaType.primary}/${mediaType.secondary}"

  def apply(mediaType: MediaType): String = s"${Printers(mediaType.tpe)}" + Printers(mediaType.parameters)

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
      case (key, None)        => key
    .mkString("&")

  def apply[A: Show](error: Error[A]): String =
    show"Error: ${error.tpe}" + error.violations.map(violations => show"\n$violations").orEmpty
