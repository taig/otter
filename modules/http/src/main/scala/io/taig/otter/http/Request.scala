package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Evidence
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.Violation
import io.taig.otter.Data
import org.typelevel.ci.*
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import java.nio.charset.Charset
import io.taig.otter.Constraint

sealed abstract class Request[A]:
  self =>

  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def bodies: Option[Bodies[?]]

  final def imap[B](f: A => B)(g: B => A): Request[B] = new Request[B]:
    export self.{bodies, headers, method, url}
    override def decode(value: Http.Request): Request.Result[B] = self.decode(value).map(f)
    override def encode(charset: Option[Charset], b: B): Http.Request = self.encode(charset, g(b))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Request[B] = imap(evidence.from)(evidence.to)

  final def zip[B](headers: Headers[B]): Request[(A, B)] = ???

  final def :*[B](header: Header[B])(using merge: Evidence.Merge[A, B]): Request[merge.Out] =
    zip(header.toHeaders).imap(merge.apply)(merge.unapply)

  final def *:[B](header: Header[B])(using merge: Evidence.Merge[B, A]): Request[merge.Out] =
    zip(header.toHeaders).imap(ab => merge(ab.swap))(merge.unapply(_).swap)

  def decode(value: Http.Request): Request.Result[A]

  def encode(charset: Option[Charset], a: A): Http.Request

object Request:
  enum Result[+A]:
    case Success(value: A)
    case ValidationViolations(violations: Codec.Error) extends Result[Nothing]
    case MediaTypesUnsupported(violations: Codec.Error) extends Result[Nothing]

    final def map[B](f: A => B): Request.Result[B] = this match
      case Success(a)                    => Success(f(a))
      case result: ValidationViolations  => result
      case result: MediaTypesUnsupported => result

    final def flatMap[B](f: A => Request.Result[B]): Request.Result[B] = this match
      case Success(a)                    => f(a)
      case result: ValidationViolations  => result
      case result: MediaTypesUnsupported => result

  def apply[A, B, C](method: Method, url: Url[A], headers: Headers[B], bodies: Bodies[C]): Request[(A, B, C)] =
    val _method = method
    val _url = url
    val _headers = headers
    val _bodies = bodies

    new Request[(A, B, C)]:
      override def method: Method = _method
      override def url: Url[A] = _url
      override def headers: Headers[B] = _headers
      override def bodies: Option[Bodies[C]] = Some(_bodies)
      override def encode(charset: Option[Charset], abc: (A, B, C)): Http.Request =
        val (mediaType, payload) = _bodies.encode(charset, abc._3)
        Http.Request(method, url.encode(abc._1), (ci"Content-Type", mediaType.print) +: headers.encode(abc._2), payload)
      override def decode(request: Http.Request): Request.Result[(A, B, C)] = request.headers
        .collectFirst { case (ci"Content-Type", value) => value }
        .fold(
          Request.Result.MediaTypesUnsupported(
            Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String("null")))
          )
        )(Request.Result.Success.apply)
        .flatMap: contentType =>
          MediaType
            .parse(contentType)
            .fold(
              Result.MediaTypesUnsupported(
                Violations.rootNec(Violation(Constraint.Type("mediaType"), actual = Data.String(contentType)))
              )
            )(Result.Success.apply)
        .flatMap: mediaType =>
          (
            url.decode(request.url),
            headers.decode(request.headers),
            _bodies.decode(mediaType, request.body)
          ).tupled match
            case Validated.Valid((a, b, Some(c))) => Result.Success((a, b, c))
            case Validated.Valid((_, _, None)) =>
              val supportedContentTypes = _bodies.toNev.map(_.mediaType.print).map(Data.String.apply).toList
              Result.MediaTypesUnsupported(
                Violations
                  .rootNec(Violation(Constraint.OneOf(supportedContentTypes), actual = Data.String(mediaType.print)))
              )
            case Validated.Invalid(violations) => Result.ValidationViolations(violations)

  def apply[A, B](method: Method, url: Url[A], headers: Headers[B]): Request[(A, B)] =
    val _method = method
    val _url = url
    val _headers = headers

    new Request[(A, B)]:
      override def method: Method = _method
      override def url: Url[A] = _url
      override def headers: Headers[B] = _headers
      override def bodies: Option[Nothing] = None
      override def encode(charset: Option[Charset], ab: (A, B)): Http.Request =
        Http.Request(method, url.encode(ab._1), headers.encode(ab._2), Http.Payload.Empty)
      override def decode(request: Http.Request): Request.Result[(A, B)] =
        (url.decode(request.url), headers.decode(request.headers)).tupled
          .fold(Result.ValidationViolations.apply, Result.Success.apply)
