package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter.Enrichment

type Endpoint[+S[_], A, B] = Enrichment[Endpoint.Value[S, A, *], B]

object Endpoint:
  final case class Value[+S[_], A, B](request: Request[S, A], response: Response[S, B]):
    def modifyRequest[S1[a] >: S[a], C](f: Request[S, A] => Request[S1, C]): Endpoint.Value[S1, C, B] =
      copy(request = f(request))

    def modifyResponse[S1[a] >: S[a], C](f: Response[S, B] => Response[S1, C]): Endpoint.Value[S1, A, C] =
      copy(response = f(response))
