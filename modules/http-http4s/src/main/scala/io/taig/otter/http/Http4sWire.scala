package io.taig.otter.http

import cats.data.Chain
import org.http4s.Entity

/** A request and a response reduced to the slices `http`'s codecs speak.
  *
  * This is the whole of what the two sides have to agree on, and writing it down is what keeps the interpreter's two
  * halves from drifting: a server reads a [[Http4sWire.Request]] and writes a [[Http4sWire.Response]], a client does
  * the reverse, and both cross to http4s through the same [[Http4sEnvelope]]. Entities retain their effect and stream;
  * only whole-document and binary schemas ask the decoder to buffer them.
  */
object Http4sWire:
  /** `body` is a pair rather than an `Option` of one because bytes always arrive, even if there are none of them: a
    * request with no entity is an empty one, and a schema that wanted a document will say so when it fails to read it.
    * The media type is optional because a sender may decline to name one. An optional body is absent only when both the
    * bytes and the media type are empty. A content type marks an empty payload as present; `Content-Length: 0` alone
    * does not distinguish it from an omitted body.
    */
  final case class Request[F[_]](
      path: Vector[String],
      queries: Chain[(String, Option[String])],
      headers: Chain[(String, String)],
      body: (Option[MediaType], Entity[F])
  )

  /** Content type remains optional so alternative selection can reject ambiguous untyped streams before consuming them.
    */
  final case class Response[F[_]](
      status: Status,
      headers: Chain[(String, String)],
      body: (Option[MediaType], Entity[F])
  )
