package io.taig.otter.http.header

trait MediaTypeDsl:
  self =>

  def apply(primary: String, secondary: String): MediaType =
    MediaType(tpe = MediaType.Type(primary, secondary), parameters = Parameters.Empty)

  object application:
    def apply(secondary: String): MediaType = self(primary = "application", secondary)

    val json: MediaType = application(secondary = "json")
    val octetStream: MediaType = application(secondary = "octet-stream")
    val wwwFormUrlencoded: MediaType = application(secondary = "x-www-form-urlencoded")

  object multipart:
    def apply(secondary: String): MediaType = self(primary = "multipart", secondary)

    val fromData: MediaType = application(secondary = "form-data")

  object text:
    def apply(secondary: String): MediaType = self(primary = "text", secondary)

    val csv: MediaType = text(secondary = "csv")
    val plain: MediaType = text(secondary = "plain")
    val html: MediaType = text(secondary = "html")

object MediaTypeDsl extends MediaTypeDsl
