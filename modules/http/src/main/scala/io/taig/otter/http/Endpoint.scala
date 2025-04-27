package io.taig.otter.http

import io.taig.otter.Metadata

final case class Endpoint[+S[_], +T[_], A, B](request: Request[S, A], response: Response[T, B], metadata: Metadata):
  def modifyRequest[U[_], C](f: Request[S, A] => Request[U, C]): Endpoint[U, T, C, B] = copy(request = f(request))

  def modifyResponse[U[_], C](f: Response[T, B] => Response[U, C]): Endpoint[S, U, A, C] = copy(response = f(response))

  def modifyMetadata(f: Metadata => Metadata): Endpoint[S, T, A, B] = copy(metadata = f(metadata))

// object Endpoint:
//   def apply[I, O](request: Request[I], response: Response[O]): Endpoint[I, O] =
//     Endpoint(request, response, Metadata.Empty)

//   given [I, O]: Metadata.Ops[Endpoint[I, O]] = new Metadata.Ops[Endpoint[I, O]]:
//     extension (self: Endpoint[I, O])
//       override def metadata: Metadata = self.metadata
//       override def modifyMetadata(f: Metadata => Metadata): Endpoint[I, O] = self.modifyMetadata(f)
