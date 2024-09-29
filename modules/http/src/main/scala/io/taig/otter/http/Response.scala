package io.taig.otter.http

import cats.data.Ior
import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.XPath
import io.taig.otter.http.header.Accept

final case class Response[A](results: Results[A], errors: Results[Route.Error], failure: Result[Unit]):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Codec.Result[Either[Route.Error, A]] = results.decode(response) match
    case Ior.Both(_, Some(a)) => a.asRight.valid
    case Ior.Right(Some(a))   => a.asRight.valid
    case Ior.Both(left, None) =>
      errors.decode(response) match
        case Ior.Both(_, Some(error)) => error.asLeft.valid
        case Ior.Both(right, None)    => left.combine(right).invalid
        case Ior.Right(Some(error))   => error.asLeft.valid
        case Ior.Right(None)          => left.invalid
        case Ior.Left(right)          => left.combine(right).invalid
    case Ior.Right(None) =>
      errors.decode(response) match
        case Ior.Both(_, Some(error)) => error.asLeft.valid
        case Ior.Both(right, None)    => right.invalid
        case Ior.Right(Some(error))   => error.asLeft.valid
        case Ior.Right(None)          => ???
        case Ior.Left(right)          => right.invalid
    case Ior.Left(violations) => violations.invalid

  def encode(accept: Option[Accept.Result], result: Either[Route.Error, A]): Http.Response = result match
    case Right(a) =>
      accept match
        case Some(accept) => results.encode(accept, a).getOrElse(contentNegotationFailed(results, accept))
        case None         =>
          // TODO should be try to infer the charset from the Content-Type header?
          results.encode(charset = none, a)
    case Left(error) =>
      accept match
        case Some(accept) => errors.encode(accept, error).getOrElse(contentNegotationFailed(errors, accept))
        case None         =>
          // TODO should be try to infer the charset from the Content-Type header?
          errors.encode(charset = none, error)

  private def contentNegotationFailed(results: Results[?], accept: Accept.Result): Http.Response =
    val mediaTypes =
      results.toNev.toList.flatMap(_.bodies.toList).flatMap(_.toNev.toList).map(_.mediaType).map(_.show).distinct
    val actual = accept match
      case Ior.Left(_)             => "null"
      case Ior.Right(mediaTypes)   => mediaTypes.map(_.show).mkString_(",")
      case Ior.Both(_, mediaTypes) => mediaTypes.map(_.show).mkString_(",")

    val violations = Violations.namespaceNec(XPath.Root / "header" / "Accept", Violation.oneOf(mediaTypes, actual))
    // TODO should we try to infer the charset from Accept and Content-Type headers?
    errors.encode(charset = none, Route.Error.ContentNegotiationFailed(violations))
