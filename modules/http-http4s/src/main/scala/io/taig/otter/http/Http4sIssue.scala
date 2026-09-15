package io.taig.otter.http

import cats.Eq
import cats.Show

/** A body this interpreter carries, which this value gave it nothing to write.
  *
  * A value rather than an exception, on the reasoning [[OpenApiIssue]] is one: it does not depend on the traffic, so a
  * caller can find it at wiring time rather than on the first request that happens to take that branch.
  *
  * It used to say more than this. An alphabet no interpreter recognised was reported here as `Uninterpreted`, and a
  * streamed body as `Streamed`, and both became a `500` describing a gap in the wiring to somebody who could do nothing
  * about it. Neither is expressible any more: the requirement parameter on every body, endpoint and route is checked
  * when routes are built, so an unregistered alphabet and a streamed body are both rejected by the compiler, and the
  * walks that used to report them no longer have a case to report from. What is left depends on the value and on
  * nothing else.
  */
enum Http4sIssue:
  /** A recognized payload could not be encoded, as a CSV document with no columns cannot be. */
  case Encoding(mediaType: MediaType, reason: String)

object Http4sIssue:
  given eq: Eq[Http4sIssue] = Eq.fromUniversalEquals

  given show: Show[Http4sIssue] =
    case Http4sIssue.Encoding(mediaType, reason) => s"Cannot encode ${mediaType.render}: $reason"
