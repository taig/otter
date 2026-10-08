package io.taig.otter.http

import io.taig.otter.Union

/** The endpoint served by a domain handler and the complete contract seen by its callers. */
final case class ComposedEndpoint[+Q[-_, +_], +S[-_, +_], -AW, +AR, -BW, +BR, +E](
    domain: Endpoint.Schema[Q, S, AW, AR, BW, BR],
    errors: ErrorPolicy[S, E]
):
  /** This endpoint as a declaration that keeps its policy under whatever defaults it is later composed with.
    *
    * Every entry of the policy becomes an override, so composing the result with an API's policy, or rendering it on
    * its own, describes exactly [[errors]] and nothing the API would have said instead.
    */
  def declaration: Endpoint.WithErrors[Q, S, AW, AR, BW, BR, E] =
    Endpoint.WithErrors(domain, ErrorOverrides.from(errors))

  /** Domain alternatives retain priority when wire representations overlap. */
  def effective: Endpoint.Schema[Q, S, AW, AR, Either[Failure, BW], Either[E, BR]] =
    val union = Union.Modify(
      Union.Coproduct(domain.responses.self.self, errors.responses.self.self),
      (value: Either[BR, E]) => value.swap,
      (value: Either[Failure, BW]) => value.swap
    )
    new Endpoint.Schema(domain.self.map(_ => Endpoint.Value(domain.request, Responses.Schema(union))))

  /** The contract a client reads. Request writers can only produce domain values, so errors are response-only here. */
  def client: Endpoint.Schema[Q, S, AW, AR, Nothing, Either[E, BR]] =
    val union = Union.Modify(
      Union.Coproduct(domain.responses.self.self, errors.responses.self.self),
      (value: Either[BR, E]) => value.swap,
      (value: Nothing) => value
    )
    new Endpoint.Schema(domain.self.map(_ => Endpoint.Value(domain.request, Responses.Schema(union))))
