package io.taig.otter.http

import io.taig.otter.http.header.ContentType

private[http] object Printers:
  def apply(contentType: ContentType): String =
    s"${contentType.tpe}/${contentType.subtype}" +
      contentType.parameters.map(parameter => s"; ${Printers(parameter)}").mkString

  def apply(parameter: ContentType.Parameter): String = s"${parameter.key}=\"${parameter.value}\""
