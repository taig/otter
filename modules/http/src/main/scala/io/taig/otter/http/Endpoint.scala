package io.taig.otter.http

import io.taig.otter.Metadata

final case class Endpoint[+S[_], +T[_], +U[_], A, B](
    request: Request[S, A],
    response: Response[T, U, B],
    metadata: Metadata
):
  def modifyRequest[V[_], C](f: Request[S, A] => Request[V, C]): Endpoint[V, T, U, C, B] = copy(request = f(request))

  def modifyResponse[V[_], U1[a] >: U[a], C](f: Response[T, U, B] => Response[V, U1, C]): Endpoint[S, V, U1, A, C] =
    copy(response = f(response))

  def modifyMetadata(f: Metadata => Metadata): Endpoint[S, T, U, A, B] = copy(metadata = f(metadata))
