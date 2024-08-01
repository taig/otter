package io.taig.otter.http

import io.taig.otter.Metadata

final case class Endpoint[I, O](request: Request[I], response: Response[O], metadata: Metadata):
  def modifyMetadata(f: Metadata => Metadata): Endpoint[I, O] = copy(metadata = f(metadata))

object Endpoint:
  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
    Endpoint(request, response, Metadata.Empty)

  given [I, O]: Metadata.Ops[Endpoint[I, O]] = new Metadata.Ops[Endpoint[I, O]]:
    extension (self: Endpoint[I, O])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Endpoint[I, O] = self.modifyMetadata(f)
