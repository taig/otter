package io.taig.otter.http

import cats.data.Validated

final class ResponseEncoder[S[_], T[_]]:
  def apply[A](response: Response[S, T, A], a: Either[Throwable, Validated[Request.Error, A]]): Response.Data = ???
