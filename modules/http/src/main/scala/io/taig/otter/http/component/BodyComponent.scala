package io.taig.otter.http.component

import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.http.Body
import io.taig.otter.http.Frame
import io.taig.otter.http.MediaType
import io.taig.otter.http.Multipart
import scodec.bits.ByteVector

/** The three forms a body comes in.
  *
  * A payload is any schema at all, so `body(MediaType.Json, someJsonSchema)` and `body(MediaType.Csv, someCsvSchema)`
  * are the same combinator, and which alphabet the document is written in is recorded in the body's type rather than
  * chosen from a fixed list here.
  */
trait BodyComponent:
  /** One document, read and written whole. */
  def apply[S[-w, +r], W, R](mediaType: MediaType, payload: => S[W, R]): Body.Schema[S, W, R] =
    Body.Schema(Body.Value.Whole(mediaType, Reference.later(payload)))

  /** Bytes, with no schema to describe them. */
  def binary(mediaType: MediaType): Body.Of[Nothing, ByteVector] =
    Body.Schema(Body.Value.Binary(mediaType))

  /** Bytes as `application/octet-stream`, which is what they are when nothing more is known. */
  val binary: Body.Of[Nothing, ByteVector] = binary(MediaType.OctetStream)

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
