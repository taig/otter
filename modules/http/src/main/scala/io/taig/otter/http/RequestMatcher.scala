package io.taig.otter.http

import cats.syntax.all.*

final class RequestMatcher[A](matcher: (Url[?], A) => Boolean):
  def apply(route: Route[?, ?, ?, ?, ?, ?], method: Method, url: A): Boolean =
    route.endpoint.request.method === method &&
      matcher(route.endpoint.request.url, url)
