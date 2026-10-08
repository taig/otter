package io.taig.otter.http

import cats.data.Chain

/** Ordered documentation declarations, the global error policy applied by consumers, and the answers to requests no
  * endpoint is addressed to.
  *
  * The unrouted answers sit beside `errors` rather than in it, because an [[ErrorPolicy]] is composed into every
  * endpoint and these belong to none. Their payload requirement still joins `S`, so whatever serves or calls this API
  * has to cover it -- the compiler checks one requirement for the whole value, not one per consumer. Where `errors` and
  * `unrouted` are written in different alphabets, ascribe the `Api` type rather than leave the two to be inferred.
  */
final case class Api[S[-_, +_], E](
    endpoints: Chain[Endpoint.Declaration.Node],
    errors: ErrorPolicy[S, E],
    unrouted: UnroutedPolicy[S]
):
  type Requirement[-W, +R] = S[W, R]
  type Error = E

  private[http] def typed: Api[Api.this.Requirement, Api.this.Error] = this

  def effective: Chain[Endpoint.Node] = endpoints.map(_.compose(errors).effective)

object Api:
  def apply[S[-_, +_], E](
      errors: ErrorPolicy[S, E],
      unrouted: UnroutedPolicy[S],
      endpoints: Endpoint.Declaration.Node*
  ): Api[S, E] = new Api(Chain.fromSeq(endpoints), errors, unrouted)
