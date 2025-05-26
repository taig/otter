package io.taig.otter.http

import io.taig.otter.Reference

final case class Endpoint[+S[_], +T[_], A, B](request: Reference[S, A], response: Reference[T, B]):
  def modifyRequest[S1[a] >: S[a], C](f: S1[A] => S1[C]): Endpoint[S1, T, C, B] =
    copy(request = request.mapF(f))

  def modifyResponse[T1[a] >: T[a], C](f: T1[B] => T1[C]): Endpoint[S, T1, A, C] =
    copy(response = response.mapF(f))
