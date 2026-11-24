package io.taig.otter.http.codec

import fs2.Stream
import io.taig.otter.http.Body

/** Independently registered whole-document and stream capabilities. */
object Http4sInterpreter:
  /** 8 MiB, as much as a server reads whole of one request body unless told otherwise. */
  val DefaultMaxBufferedBytes: Long = 8L * 1024 * 1024

  type Supported[F[_], P[-w, +r], Q[-w, +r]] =
    Body.Or[Http4sPayload.Supported[P], Body.Streamed.Requirement[Stream[F, +*], Q]]

  trait Of[P[-_, +_], Q[-_, +_]]:
    def buffered: Http4sPayload[P]
    def streams: Http4sStreams[Q]

    /** The most a server reads whole of a request body: a whole document, a binary body or a multipart one.
      *
      * A body past it is not read further and answered with [[io.taig.otter.http.ErrorPolicy.contentTooLarge]], and one
      * whose `Content-Length` already says so is answered before any of it is read. Streams are not subject to it,
      * because nothing holds them: a raw byte stream's handler sees every byte and decides for itself, and a framed
      * stream keeps the per-frame limit it was registered with.
      *
      * It belongs to the interpreter rather than to a route because it bounds what this process holds, which is the
      * server's business and not the endpoint's: the same declaration is served behind different limits and documented
      * under none. A client reads what it asked for and is not bounded by it.
      */
    def maxBufferedBytes: Long = Http4sInterpreter.DefaultMaxBufferedBytes

    /** This interpreter, reading at most `limit` bytes of a request body whole, and without bound at `Long.MaxValue`.
      *
      * Register streams first: the result is an interpreter and no longer a payload registry.
      */
    final def withMaxBufferedBytes(limit: Long): Http4sInterpreter.Of[P, Q] =
      require(limit >= 0, "maxBufferedBytes must not be negative")
      new Http4sInterpreter.Of[P, Q]:
        override def buffered: Http4sPayload[P] = Of.this.buffered
        override def streams: Http4sStreams[Q] = Of.this.streams
        override def maxBufferedBytes: Long = limit
