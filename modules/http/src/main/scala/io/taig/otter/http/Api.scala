package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*

/** Ordered endpoint definitions sharing one error vocabulary and default policy. */
final case class Api[S[-w, +r], E](
    endpoints: Chain[Endpoint.Node],
    errors: ErrorPolicy[S, E],
    private val overrides: Chain[(Endpoint.Node, ErrorOverrides[S, E])] = Chain.empty
):
  def withErrors(endpoint: Endpoint.Node, values: ErrorOverrides[S, E]): Either[ApiIssue, Api[S, E]] =
    if !endpoints.exists(_ eq endpoint) then Left(ApiIssue.Unregistered(endpoint))
    else if overrides.exists(_._1 eq endpoint) then Left(ApiIssue.DuplicateOverride(endpoint))
    else Right(copy(overrides = overrides :+ (endpoint -> values)))

  def policy(endpoint: Endpoint.Node): Either[ApiIssue, ErrorPolicy[S, E]] =
    if !endpoints.exists(_ eq endpoint) then Left(ApiIssue.Unregistered(endpoint))
    else
      Right(
        overrides
          .collectFirst { case (registered, values) if registered eq endpoint => values(errors) }
          .getOrElse(errors)
      )

  def resolve[T[-w, +r] >: S[w, r], AW, AR, BW, BR](
      endpoint: Endpoint.Schema[T, AW, AR, BW, BR]
  ): Either[ApiIssue, ComposedEndpoint[T, AW, AR, BW, BR, E]] = policy(endpoint).map(_(endpoint))

  /** Effective endpoint nodes for documentation and source generation. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def effective: Either[ApiIssue, Chain[Endpoint.Node]] =
    endpoints.traverse: endpoint =>
      policy(endpoint).map: policy =>
        policy(endpoint.asInstanceOf[Endpoint.Schema[S, Nothing, Any, Nothing, Any]]).effective

object Api:
  def apply[S[-w, +r], E](errors: ErrorPolicy[S, E], endpoints: Endpoint.Node*): Either[ApiIssue, Api[S, E]] =
    val duplicate = endpoints.indices.collectFirst:
      case index if endpoints.take(index).exists(_ eq endpoints(index)) => endpoints(index)
    duplicate.fold[Either[ApiIssue, Api[S, E]]](Right(new Api(Chain.fromSeq(endpoints), errors, Chain.empty)))(
      ApiIssue.Duplicate.apply.andThen(Left.apply)
    )

enum ApiIssue:
  case Duplicate(endpoint: Endpoint.Node)
  case DuplicateOverride(endpoint: Endpoint.Node)
  case Unregistered(endpoint: Endpoint.Node)
