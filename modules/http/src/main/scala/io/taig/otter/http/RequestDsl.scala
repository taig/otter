package io.taig.otter.http
import io.taig.otter.Merge

trait RequestDsl:
  def request[A](method: Method, url: Url[A]): Request[Nothing, A] =
    Request.Root(method, url, headers = Headers.Empty, body = Body.Empty).imap((a, _, _) => a)(a => (a, (), ()))

  def request[A, B](method: Method, url: Url[A], headers: Headers[B]): Request[Nothing, (A, B)] =
    Request.Root(method, url, headers, body = Body.Empty).imap((a, b, _) => (a, b))((a, b) => (a, b, ()))

  def request[S[_], A, B](method: Method, url: Url[A], body: Body[S, B])(using
      merge: Merge[A, B]
  ): Request[S, merge.Out] =
    Request.Root(method, url, headers = Headers.Empty, body).imap((a, _, b) => (a, b))((a, b) => (a, (), b)).merge

  def request[S[_], A, B, C](
      method: Method,
      url: Url[A],
      headers: Headers[B],
      body: Body[S, C]
  ): Request[S, (A, B, C)] =
    Request.Root(method, url, headers, body)

object RequestDsl extends RequestDsl
