package io.taig.otter.http.syntax

import io.taig.otter.Enrichment
import io.taig.otter.Merge
import io.taig.otter.http.*
import scala.annotation.targetName

trait RequestSyntax:
  def request[A](method: Method, url: Url[A]): Request[Nothing, A] =
    Enrichment(Request.Value.Root(method, url, headers = Headers.Empty).imap((a, _) => a)(a => (a, ())))

  def request[A, B](method: Method, url: Url[A], headers: Headers[B]): Request[Nothing, (A, B)] =
    Enrichment(Request.Value.Root(method, url, headers).imap((a, b) => (a, b))((a, b) => (a, b)))

  @targetName("requestHeadersBodies")
  def request[S[_], A, B, C](method: Method, url: Url[A], headers: Headers[B], bodies: Bodies[S, C])(using
      merge: Merge[A, B]
  ): Request[S, (merge.Out, C)] = Enrichment:
    Request.Value
    .Payload(self = Request.Value.Root(method, url, headers), bodies)
    .imap((a, b, c) => (merge.apply((a, b)), c))((ab, c) => (merge.unapply(ab) :* c))


  @targetName("requestHeadersBody")
  def request[S[_], A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[S, C])(using
      merge: Merge[A, B]
  ): Request[S, (merge.Out, C)] = request(method, url, headers, bodies = body.toBodies)

  @targetName("requestBodies")
  def request[S[_], A, B](method: Method, url: Url[A], bodies: Bodies[S, B])(using
      merge: Merge[A, B]
  ): Request[S, merge.Out] = Enrichment:
    Request.Value
    .Payload(self = Request.Value.Root(method, url, headers = Headers.Empty), bodies)
    .imap((a, _, b) => merge.apply((a, b))) { ab =>
      val (a, b) = merge.unapply(ab)
      (a, (), b)
    }

  @targetName("requestBody")
  def request[S[_], A, B](method: Method, url: Url[A], body: Body[S, B])(using
      merge: Merge[A, B]
  ): Request[S, merge.Out] = request(method, url, bodies = body.toBodies)

object RequestSyntax extends RequestSyntax
