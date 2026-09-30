package io.taig.otter.http

import cats.data.NonEmptyChain

/** A request no endpoint is addressed to, as the router saw it.
  *
  * Not a [[Failure]], because a failure is something an endpoint raised and an [[ErrorPolicy]] is what every endpoint
  * declares in answer to one. An unrouted request belongs to no endpoint at all, so a category for it would be a
  * response every endpoint had to declare and none could ever send -- which is the reasoning that removed the
  * interpreter category, turned around. It is answered once, for the whole API, by an [[UnroutedPolicy]].
  *
  * The two cases are the two things a router can know once nothing matched, and the second is only knowable there: that
  * the path is one some endpoint spells, under methods other than the one that arrived.
  */
sealed abstract class Unrouted:
  def method: Method

  def path: Vector[String]

object Unrouted:
  /** No endpoint spells this path, under any method. */
  final case class NotFound(override val method: Method, override val path: Vector[String]) extends Unrouted

  /** Some endpoint spells this path, and `allowed` is every method one does, in the order they were registered.
    *
    * The methods are here so a body can name them. The `Allow` header a `405` must carry is not left to a declaration
    * to remember: only the interpreter knows the set, so it is the interpreter that writes it.
    */
  final case class MethodNotAllowed(
      override val method: Method,
      override val path: Vector[String],
      allowed: NonEmptyChain[Method]
  ) extends Unrouted
