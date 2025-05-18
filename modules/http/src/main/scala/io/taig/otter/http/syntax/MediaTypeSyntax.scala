package io.taig.otter.http.syntax

import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameters

trait MediaTypeSyntax:
  object mediaType:
    def apply(primary: String, secondary: String): MediaType = MediaType(
      tpe = MediaType.Type(primary, secondary),
      parameters = Parameters.Empty
    )

    object application:
      def apply(secondary: String): MediaType = mediaType(primary = "application", secondary)

      val json: MediaType = application(secondary = "json")
      val octetStream: MediaType = application(secondary = "octet-stream")
      val wwwFormUrlencoded: MediaType = application(secondary = "x-www-form-urlencoded")

    object multipart:
      def apply(secondary: String): MediaType = mediaType(primary = "multipart", secondary)

      val fromData: MediaType = application(secondary = "form-data")

    object text:
      def apply(secondary: String): MediaType = mediaType(primary = "text", secondary)

      val csv: MediaType = text(secondary = "csv")
      val plain: MediaType = text(secondary = "plain")
      val html: MediaType = text(secondary = "html")

object MediaTypeSyntax extends MediaTypeSyntax
