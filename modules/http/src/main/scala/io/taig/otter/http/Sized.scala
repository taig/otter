package io.taig.otter.http

/** A raw byte stream and the length it declared before any of it arrived.
  *
  * The length is what the sender said, not what was counted: a reader learns it before reading a byte, which is the
  * point, and whether the stream keeps that promise can only be known once it has been read to its end. Holding it to
  * the promise is left to whoever reads it, as the size of a raw stream always is.
  */
final case class Sized[C[+_]](length: Long, bytes: C[Byte]):
  require(length >= 0, "A declared length must not be negative")
