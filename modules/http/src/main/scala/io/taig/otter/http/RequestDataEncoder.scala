package io.taig.otter.http

import io.taig.otter.http.header.MediaType

final class RequestDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val write = BodiesEncoder(encoder)

  def apply[A](request: Request[S, A], contentType: Option[MediaType], a: A): Request.Data = request match
    case Request.Modify(self, _, g) => apply(request = self, contentType, g(a))
    case Request.Payload(self, bodies) =>
      apply(request = self, contentType, a.init)
        .withBody(write(bodies, contentType, a._3).getOrElse(Array.emptyByteArray))
    case Request.Root(method, url, headers) =>
      Request.Data(
        method,
        url = UrlDataEncoder(url, a._1),
        headers = HeadersDataEncoder(headers, a._2),
        body = Array.emptyByteArray
      )
    case Request.ZipHeaders(self, headers) =>
      apply(request = self, contentType, a._1).modifyHeaders(_ ++ HeadersDataEncoder(headers, a._2))
