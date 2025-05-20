package io.taig.otter.http

import io.taig.otter.Metadata
import io.taig.otter.schema.EnrichedSchema

final case class Endpoint[+S[_], +T[_], +U[_], A, B](
    request: Request[S, A],
    response: Response[T, U, B],
    metadata: Metadata
):
  def modifyRequest[V[_], C](f: Request[S, A] => Request[V, C]): Endpoint[V, T, U, C, B] = copy(request = f(request))

  def modifyResponse[V[_], U1[a] >: U[a], C](f: Response[T, U, B] => Response[V, U1, C]): Endpoint[S, V, U1, A, C] =
    copy(response = f(response))

  def metadata(f: Metadata => Metadata): Endpoint[S, T, U, A, B] = copy(metadata = f(metadata))

object Endpoint:
  given [S[_], T[_], U[_], A]: EnrichedSchema[Endpoint[S, T, U, A, *]] = new EnrichedSchema[Endpoint[S, T, U, A, *]]:
    extension [B](self: Endpoint[S, T, U, A, B])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Endpoint[S, T, U, A, B] = self.metadata(f)

    override def imap[B, C](fa: Endpoint[S, T, U, A, B])(f: B => C)(g: C => B): Endpoint[S, T, U, A, C] =
      fa.copy(response = fa.response.imap(f)(g))
