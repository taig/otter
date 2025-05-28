package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Metadata
import io.taig.otter.operation.SchemaInvariant
import cats.Invariant
import io.taig.otter.operation.Enriched

final case class Endpoint[+S[_], A, B](value: Endpoint.Value[S, A, B], metadata: Metadata):
  def request: Request[S, A] = value.request
  def request[S1[a] >: S[a], C](f: Request[S, A] => Request[S1, C]): Endpoint[S1, C, B] =
    copy(value = value.modifyRequest(f))

  def response: Response[S, B] = value.response
  def response[S1[a] >: S[a], C](f: Response[S, B] => Response[S1, C]): Endpoint[S1, A, C] =
    copy(value = value.modifyResponse(f))

object Endpoint:
  final case class Value[+S[_], A, B](request: Request[S, A], response: Response[S, B]):
    def imap[C](f: B => C)(g: C => B): Endpoint.Value[S, A, C] = copy(response = response.imap(f)(g))

    def modifyRequest[S1[a] >: S[a], C](f: Request[S, A] => Request[S1, C]): Endpoint.Value[S1, C, B] =
      copy(request = f(request))

    def modifyResponse[S1[a] >: S[a], C](f: Response[S, B] => Response[S1, C]): Endpoint.Value[S1, A, C] =
      copy(response = f(response))

  given [S[_], A]: Invariant[Endpoint[S, A, *]] with
    override def imap[B, C](fa: Endpoint[S, A, B])(f: B => C)(g: C => B): Endpoint[S, A, C] =
      fa.copy(value = fa.value.imap(f)(g))

  given [S[_], A, B]: Enriched[Endpoint[S, A, B]] with
    override def metadata(a: Endpoint[S, A, B]): Metadata = a.metadata
    override def modifyMetadata(a: Endpoint[S, A, B])(f: Metadata => Metadata): Endpoint[S, A, B] =
      a.copy(metadata = f(a.metadata))
