package io.taig.otter.http

import cats.Eq
import cats.Show

/** Something an endpoint describes that this interpreter cannot carry.
  *
  * A value rather than an exception, on the reasoning [[OpenApiIssue]] is one: what an endpoint describes is fixed
  * before a request ever arrives, so a body written in an alphabet nothing here reads is a fact about the description
  * and not about the traffic. Reporting it as data is what lets a caller find out at wiring time rather than on the
  * first request that happens to take that branch.
  *
  * Both cases are shortfalls of this module and not of the description. An endpoint that trips one of them is well
  * formed, renders a correct OpenAPI document, and is simply waiting on an interpreter that goes further.
  */
enum Http4sIssue:
  /** A payload in an alphabet no [[io.taig.otter.http.codec.Http4sPayload]] recognises. */
  case Uninterpreted(mediaType: MediaType)

  /** A streamed body, which this interpreter does not yet carry. */
  case Streamed(mediaType: MediaType)

object Http4sIssue:
  given eq: Eq[Http4sIssue] = Eq.fromUniversalEquals

  given show: Show[Http4sIssue] =
    case Http4sIssue.Uninterpreted(mediaType) => s"No payload interpreter for ${mediaType.render}"
    case Http4sIssue.Streamed(mediaType)      => s"Streamed bodies are not interpreted yet: ${mediaType.render}"
