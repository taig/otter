package io.taig.otter.http

import io.taig.otter.Metadata

final case class Endpoint[A, B](request: Request[A], response: Response[B], metadata: Metadata):
  def modifyRequest[T](f: Request[A] => Request[T]): Endpoint[T, B] = copy(request = f(request))

  def modifyResponse[T](f: Response[B] => Response[T]): Endpoint[A, T] = copy(response = f(response))

  def modifyMetadata(f: Metadata => Metadata): Endpoint[A, B] = copy(metadata = f(metadata))

object Endpoint:
  def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
    Endpoint(request, response, Metadata.Empty)

  given [I, O]: Metadata.Ops[Endpoint[I, O]] = new Metadata.Ops[Endpoint[I, O]]:
    extension (self: Endpoint[I, O])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Endpoint[I, O] = self.modifyMetadata(f)
