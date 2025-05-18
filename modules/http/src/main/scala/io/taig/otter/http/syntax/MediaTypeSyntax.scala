package io.taig.otter.http.syntax

import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameters

trait MediaTypeSyntax:
  self =>

  def apply(primary: String, secondary: String): MediaType = MediaType(
    tpe = MediaType.Type(primary, secondary),
    parameters = Parameters.Empty
  )

  object application:
    def apply(secondary: String): MediaType = self(primary = "application", secondary)

    val json: MediaType = application(secondary = "json")
    val xWwwFormUrlencoded: MediaType = application(secondary = "x-www-form-urlencoded")

object MediaTypeSyntax extends MediaTypeSyntax
