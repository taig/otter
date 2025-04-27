package io.taig.otter.http

final case class Response[+S[_], A](result: Result[S, A], errors: Result[S, Route.Error], failure: Result[S, Unit]):
  def modifyResult[T[a] >: S[a], B](f: Result[S, A] => Result[T, B]): Response[T, B] = copy(result = f(result))

  def imap[B](f: A => B)(g: B => A): Response[S, B] = copy(result = result.imap(f)(g))
