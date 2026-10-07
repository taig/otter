package io.taig.otter.http

/** The error declarations an endpoint replaces; every missing entry inherits the API policy. */
final case class ErrorOverrides[+S[-_, +_], +E](
    envelope: Option[Response.Schema[S, Failure, E]],
    syntax: Option[Response.Schema[S, Failure, E]],
    contentType: Option[Response.Schema[S, Failure, E]],
    validation: Option[Response.Schema[S, Failure, E]],
    entityRead: Option[Response.Schema[S, Failure, E]],
    encoding: Option[Response.Schema[S, Failure, E]],
    status: Option[Response.Schema[S, Failure, E]],
    unexpected: Option[Response.Schema[S, Failure, E]]
):
  def apply[T[-w, +r] >: S[w, r], F](defaults: ErrorPolicy[T, F]): ErrorPolicy[T, E | F] = ErrorPolicy(
    envelope = envelope.getOrElse(defaults.envelope),
    syntax = syntax.getOrElse(defaults.syntax),
    contentType = contentType.getOrElse(defaults.contentType),
    validation = validation.getOrElse(defaults.validation),
    entityRead = entityRead.getOrElse(defaults.entityRead),
    encoding = encoding.getOrElse(defaults.encoding),
    status = status.getOrElse(defaults.status),
    unexpected = unexpected.getOrElse(defaults.unexpected)
  )

object ErrorOverrides:
  /** Every entry replaced, so whatever defaults these are applied to, the answers are `policy`'s own. */
  def from[S[-_, +_], E](policy: ErrorPolicy[S, E]): ErrorOverrides[S, E] = ErrorOverrides(
    envelope = Some(policy.envelope),
    syntax = Some(policy.syntax),
    contentType = Some(policy.contentType),
    validation = Some(policy.validation),
    entityRead = Some(policy.entityRead),
    encoding = Some(policy.encoding),
    status = Some(policy.status),
    unexpected = Some(policy.unexpected)
  )
