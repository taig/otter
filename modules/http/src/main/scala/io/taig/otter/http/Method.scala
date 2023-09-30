package io.taig.otter.http

import cats.Eq

opaque type Method = String

object Method:
  extension (method: Method) def toString: String = method

  val Delete: Method = "DELETE"
  val Get: Method = "GET"
  val Head: Method = "HEAD"
  val Patch: Method = "PATCH"
  val Post: Method = "POST"
  val Put: Method = "PUT"

  def apply(value: String): Method = value
  given (using eq: Eq[String]): Eq[Method] = eq
