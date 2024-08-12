package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Evidence
import io.taig.otter.Violations
import java.nio.charset.Charset
import cats.Applicative
import org.typelevel.ci.*
import io.taig.otter.Violation
import io.taig.otter.filterKeys
import io.taig.otter.http.header.MediaType
import cats.data.Validated
import cats.Traverse
import cats.Eval
import cats.Monad
import cats.Functor

sealed abstract class Request[A]:
  self =>

  def method: Method
  def url: Url[?]
  def headers: Headers[?]
  def bodies: Bodies[?]

  def matches(method: Method, url: Http.Url): Boolean =
    self.method === method && self.url.matches(url)

  final def imap[B](f: A => B)(g: B => A): Request[B] = new Request[B]:
    export self.{bodies, headers, method, url}
    override def decode[F[_]: Applicative](value: Http.Request[F]): F[Request.Result[B]] =
      self.decode(value).map(_.map(f))
    override def encode[F[_]](charset: Option[Charset], b: B): Http.Request[F] = self.encode(charset, g(b))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Request[B] = imap(evidence.from)(evidence.to)

  final def zip[B](headers: Headers[B]): Request[(A, B)] =
    val _headers = headers

    new Request[(A, B)]:
      export self.{bodies, method, url}
      override def headers: Headers[?] = self.headers.zip(headers)
      override def decode[F[_]: Applicative](value: Http.Request[F]): F[Request.Result[(A, B)]] =
        val (left, remainders) = value.headers.filterKeys(self.headers.toVector.map(_.name))
        val (right, _) = remainders.filterKeys(_headers.toVector.map(_.name))
        (self.decode(value.modifyHeaders(_ => left)), _headers.decode(right).pure[F]).tupled.map:
          case (Request.Result.Success(a), Validated.Valid(b))       => Request.Result.Success((a, b))
          case (Request.Result.Success(_), Validated.Invalid(right)) => Request.Result.ValidationViolations(right)
          case (Request.Result.ValidationViolations(left), Validated.Valid(_)) =>
            Request.Result.ValidationViolations(left)
          case (Request.Result.ValidationViolations(left), Validated.Invalid(right)) =>
            Request.Result.ValidationViolations(left.combine(right))
          case (Request.Result.MediaTypesUnsupported(left), _) => Request.Result.ValidationViolations(left)
      override def encode[F[_]](charset: Option[Charset], ab: (A, B)): Http.Request[F] =
        self.encode(charset, ab._1).modifyHeaders(_ ++ _headers.encode(ab._2))

  final def :*[B](header: Header[B])(using merge: Evidence.Merge[A, B]): Request[merge.Out] =
    zip(header.toHeaders).imap(merge.apply)(merge.unapply)

  final def *:[B](header: Header[B])(using merge: Evidence.Merge[B, A]): Request[merge.Out] =
    zip(header.toHeaders).imap(ab => merge(ab.swap))(merge.unapply(_).swap)

  def decode[F[_]: Applicative](value: Http.Request[F]): F[Request.Result[A]]

  def encode[F[_]](charset: Option[Charset], a: A): Http.Request[F]

object Request:
  enum Result[+A]:
    case Success(value: A)
    case ValidationViolations(violations: Violations) extends Result[Nothing]
    case MediaTypesUnsupported(violations: Violations) extends Result[Nothing]

    final def map[B](f: A => B): Request.Result[B] = this match
      case Success(a)                    => Success(f(a))
      case result: ValidationViolations  => result
      case result: MediaTypesUnsupported => result

    final def flatMap[B](f: A => Request.Result[B]): Request.Result[B] = this match
      case Success(a)                    => f(a)
      case result: ValidationViolations  => result
      case result: MediaTypesUnsupported => result

    final def traverse[F[_]: Applicative, B](f: A => F[B]): F[Request.Result[B]] = this match
      case Success(a)                    => f(a).map(Success.apply)
      case result: ValidationViolations  => result.pure[F]
      case result: MediaTypesUnsupported => result.pure[F]

  object Result:
    given Traverse[Request.Result] with
      override def map[A, B](fa: Request.Result[A])(f: A => B): Request.Result[B] = fa.map(f)
      override def foldLeft[A, B](fa: Request.Result[A], b: B)(f: (B, A) => B): B = fa match
        case Success(a) => f(b, a)
        case _ => b
      override def foldRight[A, B](fa: Request.Result[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
        fa match
          case Success(a) => f(a, lb)
          case _ => lb
      override def traverse[G[_]: Applicative, A, B](fa: Request.Result[A])(f: A => G[B]): G[Request.Result[B]] =
        fa.traverse(f)

  def apply[A, B, C](method: Method, url: Url[A], headers: Headers[B], bodies: Bodies[C]): Request[(A, B, C)] =
    val _method = method
    val _url = url
    val _headers = headers
    val _bodies = bodies

    new Request[(A, B, C)]:
      override def method: Method = _method
      override def url: Url[A] = _url
      override def headers: Headers[B] = _headers
      override def bodies: Bodies[C] = _bodies
      override def decode[F[_]: Applicative](request: Http.Request[F]): F[Request.Result[(A, B, C)]] =
        request.headers
          .collectFirst { case (ci"Content-Type", value) => value }
          .toRight(Violations.rootNec(Violation.tpe("string", "null")))
          .flatMap: contentType =>
            MediaType
              .parse(contentType)
              .leftMap(_ => Violations.rootNec(Violation.tpe("mediaType", actual = contentType)))
          .match
            case Right(mediaType) =>
              (url.decode(request.url), headers.decode(request.headers)).tupled match
                case Validated.Valid((a, b)) =>
                  request.body
                    .map(_bodies.decode(mediaType, _))
                    .map:
                      case Validated.Valid(Some((mediaType, c))) => ???
                      case Validated.Valid(None) =>
                        val supportedContentTypes = _bodies.toNev.toNonEmptyList.map(_.mediaType.show)
                        Result.MediaTypesUnsupported(
                          Violations.rootNec(Violation.oneOf(supportedContentTypes, actual = mediaType.show))
                        )
                      case Validated.Invalid(violations) => Result.ValidationViolations(violations)
                case Validated.Invalid(violations) => Result.ValidationViolations(violations).pure[F]
            case Left(violations) => Result.MediaTypesUnsupported(violations).pure[F]
      override def encode[F[_]](charset: Option[Charset], abc: (A, B, C)): Http.Request[F] = ???
      // val (mediaType, payload) = _bodies.encode(charset, abc._3)
      // Http.Request(method, url.encode(abc._1), (ci"Content-Type", mediaType.print) +: headers.encode(abc._2), payload)
