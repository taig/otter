package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.{+, Evidence}

sealed abstract class Result[A]:
  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]

  final def imap[B](f: A => B)(g: B => A): Result[B] = ???

  final def zip[B](headers: Headers[B]): Result[(A, B)] = ???

  final def orElse[B](result: Result[B]): Results[A + B] = toResults.orElse(result.toResults)
  def toResults: Results[A] = Results(this)

  def :+[B](result: Result[B]): Results[A + B] = orElse(result)

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Results[B] = toResults.to

  def decode(response: Http.Response): Validated[Violations, Option[A]]
  def encode(a: A): Http.Response

object Result:
  def apply[A](c: Code, b: Response.Body[A]): Result[A] = new Result[A]:
    override def code: Code = c
    override def headers: Headers[Unit] = Headers.Empty
    override def body: Response.Body[A] = b
    override def decode(response: Http.Response): Validated[Violations, Option[A]] =
      if code =!= response.code
      then none.valid
      else body.decodeWithRemainders(response.headers, response.body).map(_._2.some)
    override def encode(a: A): Http.Response =
      val (headers, payload) = body.encode(a)
      Http.Response(code, headers, payload)
