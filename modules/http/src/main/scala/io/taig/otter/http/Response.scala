package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Metadata
import io.taig.otter.Violations
import io.taig.otter.operation.Enriched
import io.taig.otter.operation.SchemaInvariant

final case class Response[+S[_], A](value: Response.Value[S, A], metadata: Metadata):
  def results: Results[S, A] = value.results
  def results[S1[a] >: S[a], B](f: Results[S1, A] => Results[S1, B]): Response[S1, B] =
    copy(value = value.modifyResults(f))

  def validation: Result[S, Violations] = value.validation
  def validation[S1[a] >: S[a]](f: Result[S1, Violations] => Result[S1, Violations]): Response[S1, A] =
    copy(value = value.modifyValidation(f))

  def failure: Result[S, Option[String]] = value.failure
  def failure[S1[a] >: S[a]](f: Result[S1, Option[String]] => Result[S1, Option[String]]): Response[S1, A] =
    copy(value = value.modifyFailure(f))

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

  given [S[_]]: SchemaInvariant[Response[S, *]] with
    override def imap[A, B](fa: Response[S, A])(f: A => B)(g: B => A): Response[S, B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Response[S, A]] = new Enriched[Response[S, A]]:
      override def metadata(a: Response[S, A]): Metadata = a.metadata
      override def modifyMetadata(a: Response[S, A])(f: Metadata => Metadata): Response[S, A] =
        a.copy(metadata = f(a.metadata))
