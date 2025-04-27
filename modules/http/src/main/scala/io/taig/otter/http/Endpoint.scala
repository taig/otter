package io.taig.otter.http

import io.taig.otter.Metadata

final case class Endpoint[+S[_], +T[_], A, B](request: Request[S, A], response: Response[T, B], metadata: Metadata):
  def modifyRequest[U[_], C](f: Request[S, A] => Request[U, C]): Endpoint[U, T, C, B] = copy(request = f(request))

  def modifyResponse[U[_], C](f: Response[T, B] => Response[U, C]): Endpoint[S, U, A, C] = copy(response = f(response))

  def modifyMetadata(f: Metadata => Metadata): Endpoint[S, T, A, B] = copy(metadata = f(metadata))
