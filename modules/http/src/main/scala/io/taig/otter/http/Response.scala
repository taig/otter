package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.{Codec, Data}
import io.taig.otter.{Violation, Violations}
import io.taig.otter.Constraint

final case class Response[A](
    results: Results[A],
    violations: Result[Violations[Violation[Constraint.Any, Data]]]
):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Codec.Result[A] = results.decode(response)

  def encode(a: Validated[Violations[Violation[Constraint.Any, Data]], A]): Http.Response =
    a.fold(violations.encode, results.encode)

object Response:
  sealed abstract class Body[A]:
    self =>

    def decode(payload: Http.Payload): Codec.Result[A]

    def encode(a: A): Http.Payload

  object Body:
    val Empty: Response.Body[Unit] = new Body[Unit]:
      override def decode(payload: Http.Payload): Codec.Result[Unit] = ???
      override def encode(a: Unit): Http.Payload = Http.Payload(Array.emptyByteArray)
