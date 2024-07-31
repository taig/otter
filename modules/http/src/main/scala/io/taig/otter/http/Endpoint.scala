package io.taig.otter.http

import io.taig.otter.Metadata

final case class Endpoint[I, O](request: Request[I], response: Response[O], metadata: Metadata)

object Endpoint:
  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
    Endpoint(request, response, Metadata.Empty)
