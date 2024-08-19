package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.http.header.Accept
import io.taig.otter.Violations
import io.taig.otter.XPath
import io.taig.otter.Violation
import cats.data.Ior

final case class Response[A](results: Results[A], errors: Results[Route.Error], failure: Result[Unit]):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Codec.Result[A] = ??? // results.decode(response)

  def encode(accept: Option[Accept.Result], result: Either[Route.Error, A]): Http.Response = result match
    case Right(a) =>
      accept match
        case Some(accept) => results.encode(accept, a).getOrElse(contentNegotationFailed(results, accept))
        case None         => results.encode(a)
    case Left(error) =>
      accept match
        case Some(accept) => errors.encode(accept, error).getOrElse(contentNegotationFailed(errors, accept))
        case None         => errors.encode(error)

  private def contentNegotationFailed(results: Results[?], accept: Accept.Result): Http.Response =
    val mediaTypes = results.toNev.toList.flatMap(_.bodies.toList).flatMap(_.toNev.toList).map(_.mediaType).map(_.show)
    val actual = accept match
      case Ior.Left(_)             => "null"
      case Ior.Right(mediaTypes)   => mediaTypes.map(_.show).mkString_(",")
      case Ior.Both(_, mediaTypes) => mediaTypes.map(_.show).mkString_(",")

    val violations = Violations.namespaceNec(XPath.Root / "header" / "Accept", Violation.oneOf(mediaTypes, actual))
    errors.encode(Route.Error.ContentNegotiationFailed(violations))
