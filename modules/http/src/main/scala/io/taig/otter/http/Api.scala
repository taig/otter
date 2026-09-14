package io.taig.otter.http

import cats.data.Chain

/** Ordered documentation declarations and the global error policy applied by consumers. */
final case class Api[+S[-w, +r], +E](
    endpoints: Chain[Endpoint.Declaration.Node],
    errors: ErrorPolicy[S, E]
):
  def effective: Chain[Endpoint.Node] = endpoints.map(_.compose(errors).effective)

object Api:
  def apply[S[-w, +r], E](errors: ErrorPolicy[S, E], endpoints: Endpoint.Declaration.Node*): Api[S, E] =
    new Api(Chain.fromSeq(endpoints), errors)
