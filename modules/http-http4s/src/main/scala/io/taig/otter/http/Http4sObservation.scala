package io.taig.otter.http

/** Diagnostic events do not choose an HTTP response. */
final case class Http4sObservation[F[_]](
    request: org.http4s.Request[F],
    endpoint: Endpoint.Node,
    event: Http4sObservation.Event
)

object Http4sObservation:
  enum Event:
    case Failed(failure: Failure)
    case ErrorResponseFailed(cause: Throwable)
    case Cancelled
