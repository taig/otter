package io.taig.otter.http

import io.taig.otter.Violations
import io.taig.otter.Enrichment

type Response[+S[_], A] = Enrichment[Response.Value[S, *], A]

object Response:
  final case class Value[+S[_], A](
      results: Results[S, A],
      validation: Result[S, Violations],
      failure: Result[S, Option[String]]
  ):
    def modifyResults[S1[a] >: S[a], B](f: Results[S, A] => Results[S1, B]): Response.Value[S1, B] =
      copy(results = f(results))

    def modifyValidation[S1[a] >: S[a]](f: Result[S, Violations] => Result[S1, Violations]): Response.Value[S1, A] =
      copy(validation = f(validation))

    def modifyFailure[S1[a] >: S[a]](
        f: Result[S, Option[String]] => Result[S1, Option[String]]
    ): Response.Value[S1, A] =
      copy(failure = f(failure))

  final case class Data(code: Code, headers: Headers.Data, body: Array[Byte]):
    def modifyHeaders(f: Headers.Data => Headers.Data): Response.Data = copy(headers = f(headers))

    def modifyBody(f: Array[Byte] => Array[Byte]): Response.Data = copy(body = f(body))
    def withBody(body: Array[Byte]): Response.Data = modifyBody(_ => body)
