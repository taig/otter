package io.taig.otter.http

import cats.Order
import cats.Show

/** A request method, spelled the way it goes on the wire.
  *
  * Not an enumeration. A method is an extension point of HTTP itself -- `PROPFIND`, `PATCH` before it was standard --
  * and a schema that could not name one would be describing a smaller protocol than the one it runs on. The constants
  * are the ones with defined semantics.
  */
final case class Method(name: String)

object Method:
  val Delete: Method = Method("DELETE")
  val Get: Method = Method("GET")
  val Head: Method = Method("HEAD")
  val Options: Method = Method("OPTIONS")
  val Patch: Method = Method("PATCH")
  val Post: Method = Method("POST")
  val Put: Method = Method("PUT")
  val Trace: Method = Method("TRACE")

  given order: Order[Method] = Order.by(_.name)

  given show: Show[Method] = Show.show(_.name)
