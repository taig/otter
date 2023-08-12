package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.schema.{+, Violations}

sealed abstract class Result[A]:
  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]
  final def imap[B](f: A => B)(g: B => A): Result[B] = ???

  final def orElse[B](result: Result[B]): Results[A + B] = ???
  def toResults: Results[A] = Results(this)

  def decode(response: Http.Response): Validated[Violations, Option[A]]
  def encode(a: A): Http.Response

object Result:
  def apply[A, B](c: Code, a: Headers[A], b: Response.Body[B]): Result[(A, B)] = new Result[(A, B)]:
    override def code: Code = c
    override def headers: Headers[A] = a
    override def body: Response.Body[B] = b
    override def decode(response: Http.Response): Validated[Violations, Option[(A, B)]] =
      if code =!= response.code
      then none.valid
      else
        headers
          .decodeWithRemainders(response.headers)
          .andThen { case (remainders, a) =>
            body.decodeWithRemainders(remainders, response.body).map(_.tupleLeft(a))
          }
          .map(_._2.some)
    override def encode(ab: (A, B)): Http.Response =
      val (additionalHeaders, payload) = body.encode(ab._2)
      Http.Response(code, headers.encode(ab._1) ++ additionalHeaders, payload)
