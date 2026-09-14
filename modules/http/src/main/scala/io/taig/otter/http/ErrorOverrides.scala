package io.taig.otter.http

/** The error declarations an endpoint replaces; every missing entry inherits the API policy. */
final case class ErrorOverrides[S[-w, +r], E](
    envelope: Option[Result.Schema[S, Failure, E]] = None,
    syntax: Option[Result.Schema[S, Failure, E]] = None,
    contentType: Option[Result.Schema[S, Failure, E]] = None,
    validation: Option[Result.Schema[S, Failure, E]] = None,
    entityRead: Option[Result.Schema[S, Failure, E]] = None,
    encoding: Option[Result.Schema[S, Failure, E]] = None,
    status: Option[Result.Schema[S, Failure, E]] = None,
    unexpected: Option[Result.Schema[S, Failure, E]] = None,
    interpreter: Option[Result.Schema[S, Failure, E]] = None
):
  def apply(defaults: ErrorPolicy[S, E]): ErrorPolicy[S, E] = ErrorPolicy(
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
