package io.taig.openapi.http

import cats.Eq
import io.taig.openapi.{Encoder, OpenApi}

opaque type Method = String

object Method:
  extension (method: Method) def toString: String = method

  def apply(value: String): Method = value

  given Encoder[Method] = OpenApi.fromString(_)
  given (using eq: Eq[String]): Eq[Method] = eq
