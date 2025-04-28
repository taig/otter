package io.taig.otter.http

import cats.syntax.all.*

final case class Route[F[_], +S[_], +T[_], +U[_], A, B](endpoint: Endpoint[S, T, U, A, B], implementation: A => F[B]):
  def modifyEndpoint[S1[a] >: S[a], T1[a] >: T[a], U1[a] >: U[a]](
      f: Endpoint[S, T, U, A, B] => Endpoint[S1, T1, U1, A, B]
  ): Route[F, S1, T1, U1, A, B] = copy(endpoint = f(endpoint))

//   def apply(request: Http.Request, onError: Throwable => F[Unit])(using
//       F: ApplicativeThrow[F]
//   ): F[Http.Response] =
//     val accept = request.headers
//       .collectFirst { case (ci"Accept", value) => value }
//       .traverse: value =>
//         Accept
//           .parse(value)
//           .toValidated
//           .leftMap(_ => Violations.namespaceNec(XPath.Root / "header" / "Accept", Violation.tpe("rfc9110", value)))
//       .map(_.map(_.toResult))

//     try {
//       accept
//         .match
//           case Validated.Valid(accept) =>
//             endpoint.request
//               .decode(request)
//               .traverse(implementation)
//               .map(endpoint.response.encode(accept, _))
//           case Validated.Invalid(violations) =>
//             endpoint.response.errors
//               .encode(charset = none, Route.Error.ContentNegotiationFailed(violations))
//               .pure[F]
//         .handleErrorWith: throwable =>
//           onError(throwable) *> accept.toOption.flatten
//             .flatMap(endpoint.response.failure.encode(_, ()))
//             .getOrElse(endpoint.response.failure.encode(charset = none, ()))
//             .pure[F]
//     } catch { throwable =>
//       onError(throwable) *> accept.toOption.flatten
//         .flatMap(endpoint.response.failure.encode(_, ()))
//         .getOrElse(endpoint.response.failure.encode(charset = none, ()))
//         .pure[F]
//     }

//   def :+(endpoint: Route[F, ?, ?]): Routes[F] = toRoutes :+ endpoint

//   def +:(endpoint: Route[F, ?, ?]): Routes[F] = endpoint +: toRoutes

//   def toRoutes: Routes[F] = Routes.one(this)

// object Route:
//   object Error:
//     object ContentNegotiationFailed:
//       val Type: String = "contentNegotiationFailed"

//       def parse(value: String): Option[Route.Error.ContentNegotiationFailed] =
//         value.split("\n", 1) match
//           case Array(error, violations) =>
//             Parsers.error.parseAll(error).toOption.filter(_ === Type) *>
//               Violations.parse(violations).toOption.map(Route.Error.ContentNegotiationFailed.apply)
//           case _ => none

//       given Show[Route.Error.ContentNegotiationFailed] = error => show"""${Printers.error(name = Type)}
//                                                                         |${error.violations}""".stripMargin

//     object MediaTypesUnsupported:
//       val Type: String = "mediaTypeUnsupported"

//       def parse(value: String): Option[Route.Error.MediaTypesUnsupported] =
//         value.split("\n", 1) match
//           case Array(error, violations) =>
//             Parsers.error.parseAll(error).toOption.filter(_ === Type) *>
//               Violations.parse(violations).toOption.map(Route.Error.MediaTypesUnsupported.apply)
//           case _ => none

//       given Show[Route.Error.MediaTypesUnsupported] = error => show"""${Printers.error(name = Type)}
//                                                                      |${error.violations}""".stripMargin

//     object ValidationViolations:
//       val Type: String = "validationViolations"

//       def parse(value: String): Option[Route.Error.ValidationViolations] =
//         value.split("\n", 1) match
//           case Array(error, violations) =>
//             Parsers.error.parseAll(error).toOption.filter(_ === Type) *>
//               Violations.parse(violations).toOption.map(Route.Error.ValidationViolations.apply)
//           case _ => none

//       given Show[Route.Error.ValidationViolations] = error => show"""${Printers.error(name = Type)}
//                                                                     |${error.violations}""".stripMargin
