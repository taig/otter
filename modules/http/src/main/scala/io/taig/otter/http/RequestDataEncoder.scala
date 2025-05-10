package io.taig.otter.http

import io.taig.otter.http.header.Accept

final class RequestDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
  val write = BodiesEncoder(encoder)

  def apply[A](request: Request[S, A], accept: Option[Accept], a: A): Request.Data = request match
    case Request.Modify(self, _, g) => apply(request = self, accept, g(a))
    case Request.Payload(self, bodies) =>
      val body = write(bodies, accept, a._3)
      apply(request = self, accept, a.init).withBody(???)
    case Request.Root(method, url, headers) =>
      Request.Data(
        method,
        url = UrlDataEncoder(url, a._1),
        headers = HeadersDataEncoder(headers, a._2),
        body = Array.emptyByteArray
      )
    case Request.ZipHeaders(self, headers) =>
      apply(request = self, accept, a._1).modifyHeaders(_ ++ HeadersDataEncoder(headers, a._2))
