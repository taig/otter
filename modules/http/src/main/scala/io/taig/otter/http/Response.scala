package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.http.header.Accept

final case class Response[A](results: Results[A], error: Results[Route.Error], failure: Result[Unit]):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Codec.Result[A] = ??? // results.decode(response)

  def encode(accept: Option[Accept.Result], result: Either[Route.Error, A]): Http.Response =
    /*
    encode(accept, a).toRight:
      val mediaTypes = toNev.toVector
        .mapFilter(_.bodies)
        .flatMap(_.toNev.toVector)
        .map(_.mediaType)
        .map(mediaType => Data.String(mediaType.show))
        .toList
      val actual =
        headers.collectFirst { case (ci"Accept", value) => value }.map(Data.String.apply).getOrElse(Data.Null)
      Violations.namespaceNec(XPath.Root / "header" / "Accept", Violation.oneOf(mediaTypes, actual))
     */
    ???
  // result match
  //   case Request.Result.Success(a) =>
  //     results.encode(accept, a).leftMap(contentNegotiationFailed.encode(accept, _)).merge
  //   case Request.Result.MediaTypesUnsupported(violations) => mediaTypesUnsupported.encode(accept, violations)
  //   case Request.Result.ValidationViolations(violations)  => validationViolations.encode(accept, violations)
