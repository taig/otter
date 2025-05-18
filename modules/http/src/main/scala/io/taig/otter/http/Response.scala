package io.taig.otter.http

import io.taig.otter.Violations

final case class Response[+S[_], +T[_], A](
    results: Results[S, A],
    validation: Result[T, Violations],
    failure: Result[T, Option[String]]
):
  def modifyResult[U[a] >: S[a], B](f: Results[S, A] => Results[U, B]): Response[U, T, B] = copy(results = f(results))

  def imap[B](f: A => B)(g: B => A): Response[S, T, B] = copy(results = results.imap(f)(g))

object Response:
  final case class Data(code: Code, headers: Headers.Data, body: Array[Byte]):
    def modifyHeaders(f: Headers.Data => Headers.Data): Response.Data = copy(headers = f(headers))

    def modifyBody(f: Array[Byte] => Array[Byte]): Response.Data = copy(body = f(body))
    def withBody(body: Array[Byte]): Response.Data = modifyBody(_ => body)
