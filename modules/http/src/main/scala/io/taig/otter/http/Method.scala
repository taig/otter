package io.taig.otter.http

import cats.Eq

opaque type Method = String

object Method:
  extension (method: Method) def toString: String = method
  def apply(value: String): Method = value
  given (using eq: Eq[String]): Eq[Method] = eq
