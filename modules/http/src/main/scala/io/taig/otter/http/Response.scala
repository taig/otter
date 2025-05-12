package io.taig.otter.http

import io.taig.otter.Violations

final case class Response[+S[_], +T[_], A](
    result: Results[S, A],
    validation: Results[T, Violations],
    failure: Results[T, Option[String]]
):
  def modifyResult[U[a] >: S[a], B](f: Results[S, A] => Results[U, B]): Response[U, T, B] = copy(result = f(result))

  def imap[B](f: A => B)(g: B => A): Response[S, T, B] = copy(result = result.imap(f)(g))

object Response:
  final case class Data(code: Code, headers: Headers.Data, body: Array[Byte]):
    def modifyBody(f: Array[Byte] => Array[Byte]): Response.Data = copy(body = f(body))
    def withBody(body: Array[Byte]): Response.Data = modifyBody(_ => body)

  object Error:
    type ContentNegotiationFailed = ContentNegotiationFailed.type
    case object ContentNegotiationFailed extends Throwable
