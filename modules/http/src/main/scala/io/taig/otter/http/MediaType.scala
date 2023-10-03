package io.taig.otter.http

import cats.Eq

opaque type MediaType = String

object MediaType:
  extension (self: MediaType) def toString: String = self
  def apply(value: String): MediaType = value

  given (using eq: Eq[String]): Eq[MediaType] = eq

  object application:
    val json: MediaType = "application/json"
    val octetStream: MediaType = "application/octet-stream"

  object text:
    val plain: MediaType = "text/plain"
