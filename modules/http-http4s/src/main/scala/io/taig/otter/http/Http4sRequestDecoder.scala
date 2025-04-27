package io.taig.otter.http
import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Collector
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.http.Http4sRequestDecoder.Data
import org.http4s.Header as Http4sHeader
import org.http4s.Method as Http4sMethod
import org.http4s.Query as Http4sQuery
import org.http4s.Request as Http4sRequest
import org.http4s.Uri as Http4sUri

final class Http4sRequestDecoder[F[_]: Concurrent, S](decode: Array[Byte] => Validated[Violations, S]):
  def apply[A](request: Request[S, A], value: Http4sRequest[F]): F[Validated[Violations, A]] = value.body.compile
    .to(Array)
    .map: body =>
      val data = Data(
        method = value.method,
        path = value.uri.path.segments,
        queries = value.uri.query.toVector,
        headers = value.headers.headers,
        body
      )

      apply(request, data).map((_, a) => a)

  def apply[A](request: Request[S, A], data: Data): Validated[Violations, (List[Http4sHeader.Raw], A)] = request match
    case Request.Modify(self, f, _) => apply(request = self, data).map(_.map(f))
    case Request.Root(method, url, headers, body) =>
      apply(method = data.method)
        .andThen: actual =>
          Validated.cond(
            test = actual === method,
            (),
            Violations.rootNec(Violation.equal(reference = method.show, actual = actual.show))
          )
        .leftMap("method" /: _) *> (
        Http4sUrlDecoder(url, path = data.path, queries = data.queries).leftMap("url" /: _),
        Http4sHeadersDecoder(headers, values = data.headers).leftMap("header" /: _),
        Http4sBodyDecoder(decode)(body, value = data.body).leftMap("body" /: _)
      ).tupled.map { case ((_, _, a), (headers, b), c) =>
        (headers, (a, b, c))
      }
    case Request.ZipHeaders(self, headers) =>
      Http4sHeadersDecoder(headers, values = data.headers) match
        case Validated.Valid((headers, b)) =>
          apply(request = self, data = data.copy(headers = headers)).map(_.tupleRight(b))
        case Validated.Invalid(url) =>
          apply(request = self, data).fold(_ |+| url, _ => url).invalid

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

object Http4sRequestDecoder:
  final case class Data(
      method: Http4sMethod,
      path: Vector[Http4sUri.Path.Segment],
      queries: Vector[Http4sQuery.KeyValue],
      headers: List[Http4sHeader.Raw],
      body: Array[Byte]
  )
