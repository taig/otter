package io.taig.otter.http

import cats.Order
import cats.Show

/** A request method, spelled the way it goes on the wire. */
opaque type Method = String

object Method:
  extension (self: Method) inline def name: String = self

  inline def apply(name: String): Method = name

  given order: Order[Method] = Order.by(_.name)

  given show: Show[Method] = Show.show(_.name)
