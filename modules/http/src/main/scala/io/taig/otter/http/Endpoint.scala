package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Metadata
import io.taig.otter.operation.SchemaInvariant

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

  given [S[_], A]: SchemaInvariant[Endpoint[S, A, *]] with
    override def imap[B, C](fa: Endpoint[S, A, B])(f: B => C)(g: C => B): Endpoint[S, A, C] =
      fa.copy(value = fa.value.imap(f)(g))

    extension [B](self: Endpoint[S, A, B])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Endpoint[S, A, B] =
        self.copy(metadata = f(self.metadata))
