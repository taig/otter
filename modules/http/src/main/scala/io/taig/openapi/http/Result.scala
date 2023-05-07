package io.taig.openapi.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.schema.{+, Violations, Void}
import io.taig.openapi.validation.Constraint

sealed abstract class Result[A]:
  def code: Code
  def headers: Headers[?]
  final def :+[B](result: Result[B]): Results[A + B] = toResults :+ result
  final def +:[B](result: Result[B]): Results[B + A] = result +: toResults
  final def imap[B](f: A => B)(g: B => A): Result[B] = Result.Modify(this, f, g)
  final def toResults: Results[A] = Results(this)
  final def decode(response: Response): Validated[Violations, A] = decodeWithRemainders(response).map(_._2)
  def decodeWithRemainders(response: Response): Validated[Violations, (Response, A)]
  def encode(a: A): Response

object Result:
  final private case class Root[A, B](code: Code, headers: Headers[A], body: Output.Body[B]) extends Result[(A, B)]:
    override def decodeWithRemainders(response: Response): Validated[Violations, (Response, (A, B))] =
      if response.code =!= code
      then
        Violations
          .oneNec(
            History.Root / "code",
            Constraint.text.equal(OpenApi.fromInt(code.toInt)).toViolation(OpenApi.fromInt(response.code.toInt))
          )
          .invalid
      else
        (headers.decodeWithRemainders(response.headers), body.decode(response.body))
          .mapN { case ((remainders, a), b) => (response.withHeaders(remainders), (a, b)) }
    override def encode(ab: (A, B)): Response = Response(code, headers.encode(ab._1), body.encode(ab._2))

  final private case class Modify[A, B](result: Result[A], f: A => B, g: B => A) extends Result[B]:
    export result.{code, headers}
    override def decodeWithRemainders(response: Response): Validated[Violations, (Response, B)] =
      result.decodeWithRemainders(response).map(_.map(f))
    override def encode(b: B): Response = result.encode(g(b))

  transparent inline def apply[A, B](code: Code, headers: Headers[A], body: Output.Body[B]): Result[?] =
    inline (headers, body) match
      case (headers: Headers[Void], body)     => Root(code, headers, body).imap { case (_, b) => b }(b => (Void, b))
      case (headers, body: Output.Body[Void]) => Root(code, headers, body).imap { case (a, _) => a }(a => (a, Void))
      case _                                  => Root(code, headers, body)
