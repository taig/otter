package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.Evidence
import io.taig.otter.Codec

sealed abstract class Result[A](val description: Option[String]):
  self =>

  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]

  final def imap[B](f: A => B)(g: B => A): Result[B] = ???

  final def zip[B](others: Headers[B]): Result[(A, B)] = new Result[(A, B)](description):
    export self.{body, code}
    override def headers: Headers[?] = self.headers.zip(others)
    override def decode(response: Http.Response): Codec.Result[Option[(A, B)]] =
      self.decode(response).andThen(_.traverse(others.decode(response.headers).tupleLeft))
    override def encode(ab: (A, B)): Http.Response =
      self.encode(ab._1).modifyHeaders(_ ++ others.encode(ab._2))

  final def :*[B](header: Header[B])(using merge: Evidence.Merge[A, B]): Result[merge.Out] =
    zip(header.toHeaders).imap(merge.apply)(merge.unapply)

  // final def orElse[B](result: Result[B]): Results[A + B] = toResults.orElse(result.toResults)
  // def toResults: Results[A] = Results(this)

  // def :+[B](result: Result[B]): Results[A + B] = orElse(result)

  // final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Results[B] = toResults.to

  def decode(response: Http.Response): Codec.Result[Option[A]]
  def encode(a: A): Http.Response

object Result:
  // extension [A <: Matchable](self: Result[A])
  //   inline def |[B <: Matchable](result: Result[B]): Results[A | B] = (self :+ result).imap {
  //     case Left(a)  => a
  //     case Right(b) => b
  //   } {
  //     case a: A => Left(a)
  //     case b: B => Right(b)
  //   }

  def apply[A](c: Code, b: Response.Body[A]): Result[A] = new Result[A](None):
    override def code: Code = c
    override def headers: Headers[Unit] = Headers.Empty
    override def body: Response.Body[A] = b
    override def decode(response: Http.Response): Codec.Result[Option[A]] =
      if code =!= response.code
      then none.valid
      else body.decode(response.headers, response.body).map(_.some)
    override def encode(a: A): Http.Response =
      val (headers, payload) = body.encode(a)
      Http.Response(code, headers, payload)
