package io.taig.otter.http

import org.http4s.Entity as Http4sBody
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.Method as Http4sMethod
import org.http4s.Request as Http4sRequest
import org.http4s.Uri as Http4sUri
import org.http4s.Query as Http4sQuery
import org.http4s.Uri as Http4sUri
import io.taig.otter.Violations
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Step
import io.taig.otter.Violation
import cats.Applicative
import cats.Apply
import cats.Monad
import cats.data.Nested

final class Http4sRequestDecoder[F[_]: Applicative, S](decode: Array[Byte] => Validated[Violations, S]):
  // TODO propagate error state down the tree to force full url & header evaluation but not decode the body
  // TODO remainders
  def apply[A](request: Request[S, A], value: Http4sRequest[F]): F[Validated[Violations, A]] =
    apply(
      request,
      method = value.method,
      path = value.uri.path.segments,
      queries = value.uri.query.toVector,
      headers = value.headers.headers,
      body = value.entity
    )
    ???
    
    // request match
    //   case Request.Modify(self, f, _) => apply(request = self, value).map(_.map(f))
    //   case Request.Root(method, url, headers, body) =>

    //     // apply(method = value.method)
    //     //   .andThen: actual =>
    //     //     Validated.cond(
    //     //       test = method === actual,
    //     //       (),
    //     //       Violations.rootNec(Violation.equal(reference = method.show, actual = actual.show))
    //     //     )
    //     //   .leftMap("method" /: _) *> (
    //     //   Http4sUrlDecoder(url, value = value.uri),
    //     //   Http4sHeadersDecoder(headers, values = value.headers),
    //     // ).tupled.traverse: (a, b) =>
    //     //   Http4sBodyDecoder(decode)(body, value = value.entity).map(c => c.map((a, b, _)))
    //     //   val x: F[Validated[Violations, Int]] = ???
    //     //   x

    //     ???
    //   case Request.ZipHeaders(self, headers) =>
    //     Http4sHeadersDecoder(headers, values = value.headers) match
    //       case Validated.Valid(b) => apply(request = self, value).map(_.tupleRight(b))
    //       case violations @ Validated.Invalid(_) => violations.pure
    //   case Request.ZipUrl(self, url) =>
    //     (apply(request = self, value), Http4sUrlDecoder(url, value = value.uri)).tupled

  def apply[A](
    request: Request[S, A],
    method: Http4sMethod,
    path: Vector[Http4sUri.Path.Segment],
    queries: Vector[Http4sQuery.KeyValue],
    headers: List[Http4sHeader.Raw],
    body: Http4sBody[F]
  ): Validated[Violations, (Vector[Http4sUri.Path.Segment], Vector[Http4sQuery.KeyValue], List[Http4sHeader.Raw], A)] =
    request match
      case Request.Modify(self, f, _) => apply(request = self, method, path, queries, headers, body).map(_.map(f))

  def apply(method: Http4sMethod): Validated[Violations, Method] = method match
    case Http4sMethod.DELETE  => Method.Delete.valid
    case Http4sMethod.GET     => Method.Get.valid
    case Http4sMethod.HEAD    => Method.Head.valid
    case Http4sMethod.OPTIONS => Method.Options.valid
    case Http4sMethod.PATCH   => Method.Patch.valid
    case Http4sMethod.POST    => Method.Post.valid
    case Http4sMethod.PUT     => Method.Put.valid
    case Http4sMethod.TRACE   => Method.Trace.valid
    case _ =>
      val values = Method.mapping.values.toList.map(Method.mapping.apply)
      Violations.rootNec(Violation.oneOf(values, actual = method.name)).invalid
