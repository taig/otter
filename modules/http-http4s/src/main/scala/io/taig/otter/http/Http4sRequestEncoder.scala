// package io.taig.otter.http

// import org.http4s.Entity as Http4sBody
// import org.http4s.Headers as Http4sHeaders
// import org.http4s.Method as Http4sMethod
// import org.http4s.Request as Http4sRequest
// import org.http4s.Uri as Http4sUri

// final class Http4sRequestEncoder[F[_], S[_]](encode: [A] => S[A] => String):
//   self =>

//   val body: Http4sBodyEncoder[F, S] = Http4sBodyEncoder(???)

//   def apply[A](request: Request[S, A], a: A): Http4sRequest[F] =
//     val (method, body) = root(request, a)
//     val url = this.url(request, a)
//     val headers = this.headers(request, a)
//     Http4sRequest(method, url, headers = headers, entity = body)

//   def root[A](request: Request[S, A], a: A): (Http4sMethod, Http4sBody[F]) = request match
//     case Request.Modify(self, _, g) => root(request = self, g(a))
//     case Request.Root(method, _, _, body) =>
//       (toHttp4sMethod(method), self.body(headers = ???, body, a._3))
//     case Request.ZipHeaders(self, _) => root(request = self, a._1)

//   def toHttp4sMethod(method: Method): Http4sMethod = method match
//     case Method.Delete  => Http4sMethod.DELETE
//     case Method.Get     => Http4sMethod.GET
//     case Method.Head    => Http4sMethod.HEAD
//     case Method.Options => Http4sMethod.OPTIONS
//     case Method.Patch   => Http4sMethod.PATCH
//     case Method.Post    => Http4sMethod.POST
//     case Method.Put     => Http4sMethod.PUT
//     case Method.Trace   => Http4sMethod.TRACE

//   def headers[A](request: Request[S, A], a: A): Http4sHeaders = request match
//     case Request.Modify(self, _, g)               => headers(request = self, g(a))
//     case Request.Root(method, url, headers, body) => Http4sHeadersEncoder(codec = headers, a._2)
//     case Request.ZipHeaders(self, headers) =>
//       this.headers(request = self, a._1) ++ Http4sHeadersEncoder(codec = headers, a._2)

//   def url[A](request: Request[S, A], a: A): Http4sUri = request match
//     case Request.Modify(self, _, g)  => url(request = self, g(a))
//     case Request.Root(_, url, _, _)  => Http4sUrlEncoder(url, a._1)
//     case Request.ZipHeaders(self, _) => url(request = self, a._1)
