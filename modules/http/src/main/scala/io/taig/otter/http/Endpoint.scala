package io.taig.otter.http

final case class Endpoint[+S[_], A, B](request: Request[S, A], response: Response[S, B]):
  def modifyRequest[S1[a] >: S[a], C](f: Request[S1, A] => Request[S1, C]): Endpoint[S1, C, B] =
    copy(request = f(request))

  def modifyResponse[S1[a] >: S[a], C](f: Response[S1, B] => Response[S1, C]): Endpoint[S1, A, C] =
    copy(response = f(response))

object Endpoint
