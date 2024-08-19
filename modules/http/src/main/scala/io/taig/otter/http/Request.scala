package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Violations
import java.nio.charset.Charset
import org.typelevel.ci.*
import io.taig.otter.Violation
import io.taig.otter.filterKeys
import io.taig.otter.http.header.MediaType
import cats.data.Validated
import io.taig.otter.XPath
import io.taig.otter.Convert
import io.taig.otter.Merge

sealed abstract class Request[A]:
  self =>

  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def bodies: Option[Bodies[?]]

  final def matches(method: Method, url: Http.Url): Boolean = self.method === method && self.url.matches(url)

  final def imap[B](f: A => B)(g: B => A): Request[B] = new Request[B]:
    export self.{bodies, headers, method, url}
    override def decode(contentType: MediaType, request: Http.Request): Either[Route.Error, B] =
      self.decode(contentType, request).map(f)
    override def encode(charset: Option[Charset], b: B): Http.Request = self.encode(charset, g(b))

  final def to[B](using convert: Convert[A, B]): Request[B] = imap(convert.to)(convert.from)

  final def zip[B](headers: Headers[B]): Request[(A, B)] =
    val _headers = headers

    new Request[(A, B)]:
      export self.{bodies, method, url}
      override def headers: Headers[?] = self.headers.zip(headers)
      override def decode(contentType: MediaType, request: Http.Request): Either[Route.Error, (A, B)] =
        val (left, remainders) = request.headers.filterKeys(self.headers.toVector.map(_.name))
        val (right, _) = remainders.filterKeys(_headers.toVector.map(_.name))
        (self.decode(contentType, request.modifyHeaders(_ => left)), _headers.decode(right)) match
          case (Right(a), Validated.Valid(b))       => Right((a, b))
          case (Right(_), Validated.Invalid(right)) => Left(Route.Error.ValidationViolations(right))
          case (Left(Route.Error.ValidationViolations(left)), Validated.Invalid(right)) =>
            Left(Route.Error.ValidationViolations(left.combine(right)))
          case (left@Left(_), _) => left.asInstanceOf[Either[Route.Error, (A, B)]]
      override def encode(charset: Option[Charset], ab: (A, B)): Http.Request =
        self.encode(charset, ab._1).modifyHeaders(_ ++ _headers.encode(ab._2))

  final def :*[B](header: Header[B])(using merge: Merge[A, B]): Request[merge.Out] =
    zip(header.toHeaders).imap(merge.apply)(merge.unapply)

  final def *:[B](header: Header[B])(using merge: Merge[B, A]): Request[merge.Out] =
    zip(header.toHeaders).imap(ab => merge(ab.swap))(merge.unapply(_).swap)

  final def decode(request: Http.Request): Either[Route.Error, A] = request.headers
    .collectFirst { case (ci"Content-Type", value) => value }
    .toRight(Violations.namespaceNec(XPath.Root / "header" / "Content-Type", Violation.tpe("string", "null")))
    .flatMap: contentType =>
      MediaType
        .parse(contentType)
        .leftMap(_ =>
          Violations.namespaceNec(XPath.Root / "header" / "Content-Type", Violation.tpe("mediaType", contentType))
        )
    .match
      case Right(mediaType) => decode(mediaType, request)
      case Left(violations) => ??? // Request.Result.MediaTypesUnsupported(violations)

  def decode(contentType: MediaType, request: Http.Request): Either[Route.Error, A]

  def encode(charset: Option[Charset], a: A): Http.Request

object Request:
  def apply[A, B, C](method: Method, url: Url[A], headers: Headers[B], bodies: Bodies[C]): Request[(A, B, C)] =
    val _method = method
    val _url = url
    val _headers = headers
    val _bodies = bodies

    new Request[(A, B, C)]:
      override def method: Method = _method
      override def url: Url[A] = _url
      override def headers: Headers[B] = _headers
      override def bodies: Option[Bodies[C]] = _bodies.some
      override def decode(
          contentType: MediaType,
          request: Http.Request
      ): Either[Route.Error, (A, B, C)] = (url.decode(request.url), headers.decode(request.headers)).tupled match
        case Validated.Valid((a, b)) =>
          _bodies
            .decode(contentType, request.body) match
            case Validated.Valid(Some((_, c))) => (a, b, c).asRight
            case Validated.Valid(None) =>
              val supportedContentTypes = _bodies.toNev.toList.map(_.mediaType.show)
              Route.Error
                .MediaTypesUnsupported(
                  Violations.rootNec(Violation.oneOf(supportedContentTypes, actual = contentType.show))
                )
                .asLeft
            case Validated.Invalid(violations) =>
              Route.Error.ValidationViolations("body" /: violations).asLeft
        case Validated.Invalid(violations) => Route.Error.ValidationViolations(violations).asLeft
      override def encode(charset: Option[Charset], abc: (A, B, C)): Http.Request = ???
      // val (mediaType, payload) = _bodies.encode(charset, abc._3)
      // Http.Request(method, url.encode(abc._1), (ci"Content-Type", mediaType.print) +: headers.encode(abc._2), payload)

  def apply[A, B](method: Method, url: Url[A], headers: Headers[B]): Request[(A, B)] =
    val _method = method
    val _url = url
    val _headers = headers

    new Request[(A, B)]:
      override def method: Method = _method
      override def url: Url[A] = _url
      override def headers: Headers[B] = _headers
      override def bodies: Option[Bodies[?]] = none
      override def decode(contentType: MediaType, request: Http.Request): Either[Route.Error, (A, B)] = ???
      // (url.decode(request.url), headers.decode(request.headers)).tupled
      //   .fold(Request.Result.ValidationViolations.apply, Request.Result.Success.apply)
      override def encode(charset: Option[Charset], abc: (A, B)): Http.Request = ???
      // val (mediaType, payload) = _bodies.encode(charset, abc._3)
      // Http.Request(method, url.encode(abc._1), (ci"Content-Type", mediaType.print) +: headers.encode(abc._2), payload)
