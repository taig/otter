package io.taig.otter.http

import cats.Order
import cats.Show

import scala.collection.immutable.ListMap

/** What a body is written as.
  *
  * The parameters are kept because two of them change what the bytes mean rather than merely describing them: a
  * `charset` says how text became bytes, and a `boundary` says where one [[Multipart]] part ends. They are carried in
  * declaration order, so the rendering is stable and a document written from a schema does not change between runs.
  *
  * There is no `MediaRange` here and no `Accept`. Choosing between the alternatives a [[Bodies]] offers happens when a
  * request is in hand, which is a backend's business; describing that they exist is this module's.
  */
final case class MediaType(primary: String, secondary: String, parameters: ListMap[String, String]):
  /** The type without its parameters, which is what two media types are the same one by. */
  def essence: MediaType = MediaType(primary, secondary)

  def parameter(name: String): Option[String] = parameters.get(name)

  def withParameter(name: String, value: String): MediaType =
    MediaType(primary, secondary, parameters.updated(name, value))

  def render: String =
    parameters.foldLeft(s"$primary/$secondary")((rendered, parameter) => s"$rendered; ${parameter._1}=${parameter._2}")

object MediaType:
  def apply(primary: String, secondary: String): MediaType =
    MediaType(primary, secondary, ListMap.empty)

  val Json: MediaType = MediaType("application", "json")

  /** Newline delimited JSON, which is what a streamed sequence of JSON values is written as. */
  val NdJson: MediaType = MediaType("application", "x-ndjson")

  val Csv: MediaType = MediaType("text", "csv")

  val EventStream: MediaType = MediaType("text", "event-stream")

  val FormUrlencoded: MediaType = MediaType("application", "x-www-form-urlencoded")

  val Html: MediaType = MediaType("text", "html")

  val MultipartFormData: MediaType = MediaType("multipart", "form-data")

  val OctetStream: MediaType = MediaType("application", "octet-stream")

  val Pdf: MediaType = MediaType("application", "pdf")

  val Text: MediaType = MediaType("text", "plain")

  val Xml: MediaType = MediaType("application", "xml")

  given order: Order[MediaType] = Order.by(mediaType => (mediaType.primary, mediaType.secondary, mediaType.render))

  given show: Show[MediaType] = Show.show(_.render)
