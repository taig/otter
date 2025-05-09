package io.taig.otter.http

import cats.syntax.all.*

object RequestMatcher:
  def apply(request: Request[?, ?], data: Request.Data): Boolean =
    request.method === data.method && UrlMatcher(url = request.url, data = data.url)
