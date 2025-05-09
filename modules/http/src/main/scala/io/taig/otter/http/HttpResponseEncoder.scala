package io.taig.otter.http

final class HttpResponseEncoder[S[_], T[_]]:
  def apply[A](resonse: Response[S, T, A], a: A): Array[Byte] = ???
