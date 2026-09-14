package io.taig.otter.http

/** The error declarations an endpoint replaces; every missing entry inherits the API policy. */
final case class ErrorOverrides[+S[-w, +r], +E](
    envelope: Option[Response.Schema[S, Failure, E]] = None,
    syntax: Option[Response.Schema[S, Failure, E]] = None,
    contentType: Option[Response.Schema[S, Failure, E]] = None,
    validation: Option[Response.Schema[S, Failure, E]] = None,
    entityRead: Option[Response.Schema[S, Failure, E]] = None,
    encoding: Option[Response.Schema[S, Failure, E]] = None,
    status: Option[Response.Schema[S, Failure, E]] = None,
    unexpected: Option[Response.Schema[S, Failure, E]] = None,
    interpreter: Option[Response.Schema[S, Failure, E]] = None
):
  def apply[T[-w, +r] >: S[w, r], F](defaults: ErrorPolicy[T, F]): ErrorPolicy[T, E | F] = ErrorPolicy(
    envelope.getOrElse(defaults.envelope),
    syntax.getOrElse(defaults.syntax),
    contentType.getOrElse(defaults.contentType),
    validation.getOrElse(defaults.validation),
    entityRead.getOrElse(defaults.entityRead),
    encoding.getOrElse(defaults.encoding),
    status.getOrElse(defaults.status),
    unexpected.getOrElse(defaults.unexpected),
    interpreter.getOrElse(defaults.interpreter)
  )
