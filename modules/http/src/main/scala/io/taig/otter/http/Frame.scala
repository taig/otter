package io.taig.otter.http

import cats.Eq

/** Where one element of a streamed body ends and the next begins.
  *
  * This is the whole of what a description can say about streaming, and it is enough: an interpreter that knows the
  * framing and the element's schema can read the elements out, and nothing else about the stream is the schema's
  * business. What a sequence of them *is* -- an `fs2.Stream`, a `ZStream`, an async iterator -- is named by whoever
  * interprets the body, which is why no effect type appears here or anywhere else in this module.
  */
enum Frame:
  /** One element per line, which is what `application/x-ndjson` is. */
  case Lines

  /** Server sent events, where an element is a `data:` block terminated by a blank line. */
  case Events

  /** No framing at all: the elements are the bytes, and the schema of one is the schema of a byte. */
  case Raw

  /** Elements separated by a literal, which is the general case the other three are conventions of. */
  case Delimited(separator: String)

object Frame:
  given eq: Eq[Frame] = Eq.fromUniversalEquals
