package io.taig.otter.http

import cats.data.Chain
import scodec.bits.ByteVector

/** A request and a response reduced to the slices `http`'s codecs speak.
  *
  * This is the whole of what the two sides have to agree on, and writing it down is what keeps the interpreter's two
  * halves from drifting: a server reads a [[Http4sWire.Request]] and writes a [[Http4sWire.Response]], a client does
  * the reverse, and both cross to http4s through the same [[Http4sEnvelope]]. Nothing here mentions an effect type,
  * which is the point -- everything above this line is pure, and `F` appears only where the bytes are actually read.
  */
object Http4sWire:
  /** `body` is a pair rather than an `Option` of one because bytes always arrive, even if there are none of them: a
    * request with no entity is an empty one, and a schema that wanted a document will say so when it fails to read it.
    * The media type is optional because a sender may decline to name one.
    */
  final case class Request(
      path: Vector[String],
      queries: Chain[(String, Option[String])],
      headers: Chain[(String, String)],
      body: (Option[MediaType], ByteVector)
  )

  /** `body` is optional here because a result genuinely may have none -- a `204`, a `404` -- and unlike a request,
    * which is handed whatever arrived, this side chooses.
    */
  final case class Response(
      code: Code,
      headers: Chain[(String, String)],
      body: Option[(MediaType, ByteVector)]
  )
