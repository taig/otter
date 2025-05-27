package io.taig.otter.http

import io.taig.otter.Violations
import io.taig.otter.operation.EnrichedSchemaInvariant
import io.taig.otter.Metadata
import io.taig.otter.Enrichment
import cats.syntax.all.*

type Response[+S[_], A] = Enrichment[Response.Value[S, *], A]

object Response:
  final case class Value[+S[_], A](
      results: Results[S, A],
      validation: Result[S, Violations],
      failure: Result[S, Option[String]]
  ):
    final def imap[B](f: A => B)(g: B => A): Response.Value[S, B] = copy(results = results.imap(f)(g))

    def modifyResults[S1[a] >: S[a], B](f: Results[S, A] => Results[S1, B]): Response.Value[S1, B] =
      copy(results = f(results))

    def modifyValidation[S1[a] >: S[a]](f: Result[S, Violations] => Result[S1, Violations]): Response.Value[S1, A] =
      copy(validation = f(validation))

    def modifyFailure[S1[a] >: S[a]](
        f: Result[S, Option[String]] => Result[S1, Option[String]]
    ): Response.Value[S1, A] = copy(failure = f(failure))

  final case class Data(code: Code, headers: Headers.Data, body: Array[Byte]):
    def modifyHeaders(f: Headers.Data => Headers.Data): Response.Data = copy(headers = f(headers))

    def modifyBody(f: Array[Byte] => Array[Byte]): Response.Data = copy(body = f(body))
    def withBody(body: Array[Byte]): Response.Data = modifyBody(_ => body)

  extension [S[_], A](self: Response[S, A])
    def results: Results[S, A] = self.self.results
    def validation: Result[S, Violations] = self.self.validation
    def failure: Result[S, Option[String]] = self.self.failure

  given [S[_]]: EnrichedSchemaInvariant[Response[S, *]] with
    override def imap[A, B](fa: Response[S, A])(f: A => B)(g: B => A): Response[S, B] =
      fa.mapF(_.imap(f)(g))

    extension [A](self: Response[S, A])
      override def metadata: Metadata = self.metadata
      override def metadata(f: Metadata => Metadata): Response[S, A] = self.modifyMetadata(f)
