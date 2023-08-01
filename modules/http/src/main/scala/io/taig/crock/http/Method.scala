package io.taig.crock.http

import cats.Eq

opaque type Method = String

object Method:
  extension (method: Method) def toString: String = method

  def apply(value: String): Method = value

  given Encoder[Method] = OpenApi.fromString(_)
  given (using eq: Eq[String]): Eq[Method] = eq
