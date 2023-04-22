package io.taig.openapi.http

import cats.Eq

opaque type Method = String

object Method:
  def apply(value: String): Method = value

  extension (method: Method) def toString: String = method

  given (using eq: Eq[String]): Eq[Method] = eq
