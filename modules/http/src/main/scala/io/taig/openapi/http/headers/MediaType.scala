package io.taig.openapi.http.headers

import cats.Eq

opaque type MediaType = String

object MediaType:
  extension (self: MediaType) def toString: String = self
  def apply(value: String): MediaType = value

  given (using eq: Eq[String]): Eq[MediaType] = eq

  object text:
    val plain: MediaType = "text/plain"
