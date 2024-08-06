package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.{Codec, Data}
import io.taig.otter.{Violation, Violations}
import io.taig.otter.Constraint
import io.taig.otter.http.Http.Payload

final case class Response[A](
    results: Results[A],
    error: Result[Request.Error],
    violations: Result[Violations[Violation[Constraint.Any, Data]]]
):
  final def modifyResults[T](f: Results[A] => Results[T]): Response[T] = copy(results = f(results))

  def decode(response: Http.Response): Codec.Result[A] = results.decode(response)

  def encode(a: Validated[Violations[Violation[Constraint.Any, Data]], A]): Http.Response =
    a.fold(violations.encode, results.encode)

object Response:
  sealed abstract class Body[A]:
    self =>

    def mediaType: MediaType

    final def toResponseBodies: Response.Bodies[A] = Bodies(this)

    def decode(payload: Http.Payload): Codec.Result[A]

    def encode(a: A): (Http.Headers, Http.Payload)

  sealed abstract class Bodies[A]:
    self =>

    def toVector: Vector[Response.Body[?]]

    final def orElse[B](bodies: Response.Bodies[B]): Response.Bodies[Either[A, B]] = new Bodies[Either[A, B]]:
      override def toVector: Vector[Body[?]] = self.toVector ++ bodies.toVector
      override def decode(mediaType: MediaType, payload: Payload): Codec.Result[Option[Either[A, B]]] =
        self
          .decode(mediaType, payload)
          .map(_.map(_.asLeft))
          .andThen:
            case a @ Some(_) => a.valid
            case None        => bodies.decode(mediaType, payload).map(_.map(_.asRight))
      override def encode(ab: Either[A, B]): (Http.Headers, Payload) = ab.fold(self.encode, bodies.encode)

    def decode(mediaType: MediaType, payload: Http.Payload): Codec.Result[Option[A]]

    def encode(a: A): (Http.Headers, Http.Payload)

  object Bodies:
    def apply[A](body: Response.Body[A]): Response.Bodies[A] = new Bodies[A]:
      override def toVector: Vector[Body[?]] = Vector(body)
      override def decode(mediaType: MediaType, payload: Payload): Codec.Result[Option[A]] =
        if body.mediaType =!= mediaType then none.valid else body.decode(payload).map(_.some)
      override def encode(a: A): (Http.Headers, Payload) = body.encode(a)
