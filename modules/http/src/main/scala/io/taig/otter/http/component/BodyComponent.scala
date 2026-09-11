package io.taig.otter.http.component

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.http.Bodies
import io.taig.otter.http.Body
import io.taig.otter.http.Frame
import io.taig.otter.http.MediaType
import io.taig.otter.http.Multipart
import scodec.bits.ByteVector

import scala.annotation.targetName

/** The three forms a body comes in.
  *
  * A payload is any schema at all, so `body(MediaType.Json, someJsonSchema)` and `body(MediaType.Csv, someCsvSchema)`
  * are the same combinator, and which alphabet the document is written in is recorded in the body's type rather than
  * chosen from a fixed list here.
  *
  * [[optional]] is not a fourth form. It is the one thing a position taking a body has to be told that the body cannot
  * say for itself, and it is spelled here so that `request(method.post, path)(body.optional(entity))` needs no second
  * name for the position -- which is what [[Bodies.Optional]] records.
  *
  * Its two overloads carry a `@targetName` for the reason [[io.taig.otter.http.syntax.EndpointSyntax]]'s do: a by-name
  * parameter erases to `Function0`, and a body and a choice of them are then one signature.
  */
trait BodyComponent:
  /** One document, read and written whole. */
  def apply[S[-w, +r], W, R](mediaType: MediaType, payload: => S[W, R]): Body.Schema[S, W, R] =
    Body.Schema(Body.Value.Whole(mediaType, Reference.later(payload)))

  /** Bytes, with no schema to describe them. */
  def binary(mediaType: MediaType): Body.Of[Body.Opaque, ByteVector] =
    Body.Schema(Body.Value.Binary(mediaType))

  /** Bytes as `application/octet-stream`, which is what they are when nothing more is known. */
  val binary: Body.Of[Body.Opaque, ByteVector] = binary(MediaType.OctetStream)

  /** A sequence of documents, arriving one at a time.
    *
    * The result carries the element type so that a backend can pin it; `.body` is the same body as the request holds
    * it, which is as something contributing nothing to what the request reads.
    */
  def streamed[S[-w, +r], W, R](
      mediaType: MediaType,
      frame: Frame,
      element: => S[W, R]
  ): Body.Streamed.Schema[S, W, R] =
    new Body.Streamed.Schema(Annotation(Body.Value.Streamed(mediaType, frame, Reference.later(element))))

  /** Newline delimited JSON, which is what a streamed sequence of documents is written as by default. */
  def streamed[S[-w, +r], W, R](element: => S[W, R]): Body.Streamed.Schema[S, W, R] =
    streamed(MediaType.NdJson, Frame.Lines, element)

  /** A body whose content is a set of parts.
    *
    * A [[Multipart]] schema is a payload like any other, so this is [[apply]] with the media type filled in. The
    * boundary is not named here: it is generated per request, so a schema that fixed one would be describing a
    * different upload every time it was sent.
    */
  def multipart[B[-w, +r], W, R](
      parts: => Multipart.Schema[B, W, R]
  ): Body.Schema[[w, r] =>> Multipart.Schema[B, w, r], W, R] = apply(MediaType.MultipartFormData, parts)

  /** A body that need not be sent at all. */
  @targetName("body")
  def optional[S[-w, +r], W, R](value: => Body.Schema[S, W, R]): Bodies.Optional[S, W, R] =
    optional(Bodies.Schema.apply[S, W, R](Self.Union.Root(Reference.later(value))))

  /** A choice of bodies, none of which need be sent at all. */
  @targetName("bodies")
  def optional[S[-w, +r], W, R](values: => Bodies.Schema[S, W, R]): Bodies.Optional[S, W, R] =
    Bodies.Optional(Reference.later(values))
