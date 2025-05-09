package io.taig.otter.http

final class HttpRequestEncoder[S[_]]:
  def apply[A](request: Request[S, A], a: A): Array[Byte] = ???
