package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.Metadata
import io.taig.otter.Enrichment
import io.taig.otter.operation.EnrichedSchemaInvariant
import cats.syntax.all.*

type Endpoint[+S[_], A, B] = Enrichment[Endpoint.Value[S, A, *], B]

object Endpoint:
  final case class Value[+S[_], A, B](request: Request[S, A], response: Response[S, B]):
    def imap[C](f: B => C)(g: C => B): Endpoint.Value[S, A, C] = copy(response = response.imap(f)(g))

    def modifyRequest[S1[a] >: S[a], C](f: Request[S, A] => Request[S1, C]): Endpoint.Value[S1, C, B] =
      copy(request = f(request))

    def modifyResponse[S1[a] >: S[a], C](f: Response[S, B] => Response[S1, C]): Endpoint.Value[S1, A, C] =
      copy(response = f(response))

  extension [S[_], A, B](self: Endpoint[S, A, B])
    def request: Request[S, A] = self.self.request
    def response: Response[S, B] = self.self.response

  given [S[_], A]: EnrichedSchemaInvariant[Endpoint[S, A, *]] with
    override def imap[B, C](fa: Endpoint[S, A, B])(f: B => C)(g: C => B): Endpoint[S, A, C] = fa.mapF(_.imap(f)(g))

    extension [B](self: Endpoint[S, A, B])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Endpoint[S, A, B] = self.modifyMetadata(f)
