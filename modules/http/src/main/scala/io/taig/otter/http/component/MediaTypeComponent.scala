package io.taig.otter.http.component

import io.taig.otter.http.MediaType

/** The media types worth having a name for.
  *
  * Here rather than on [[MediaType]], for the reason [[MethodComponent]] gives: the type says what a media type is, and
  * the dsl says which ones are worth spelling as a word. [[apply]] names the rest, so the list being only the types a
  * body is commonly written in costs nothing.
  *
  * Unlike a method or a status code, a media type is also named by this library itself -- a JSON body knows it is
  * `application/json` without being told -- which is what the companion object below is for. A caller reaches these
  * through the dsl as `mediaType.json`; the modules that build a body reach the same values without going through a
  * vocabulary they are not part of.
  */
trait MediaTypeComponent:
  /** A media type by its two halves, which is how a type this list has never heard of is still named. */
  def apply(primary: String, secondary: String): MediaType = MediaType(primary, secondary)

  val json: MediaType = MediaType("application", "json")

  /** Newline delimited JSON, which is what a streamed sequence of JSON values is written as. */
  val ndJson: MediaType = MediaType("application", "x-ndjson")

  val csv: MediaType = MediaType("text", "csv")

  val eventStream: MediaType = MediaType("text", "event-stream")

  val formUrlencoded: MediaType = MediaType("application", "x-www-form-urlencoded")

  val html: MediaType = MediaType("text", "html")

  val multipartFormData: MediaType = MediaType("multipart", "form-data")

  val octetStream: MediaType = MediaType("application", "octet-stream")

  val pdf: MediaType = MediaType("application", "pdf")

  val text: MediaType = MediaType("text", "plain")

  val xml: MediaType = MediaType("application", "xml")

object MediaTypeComponent extends MediaTypeComponent
