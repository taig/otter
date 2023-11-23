package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.{+, Evidence}

sealed abstract class Result[A](val description: Option[String]):
  self =>
  def code: Code
  def headers: Headers[?]
  def body: Response.Body[?]

  def description(f: Option[String] => Option[String]): Result[A] =
    new Result[A](f(description)) { export self.* }
  def description(value: Option[String]): Result[A] = description(_ => value)
  def description(value: String): Result[A] = description(Some(value))

  final def imap[B](f: A => B)(g: B => A): Result[B] = new Result[B](description):
    export self.{body, code, headers}
    override def decode(response: Http.Response): Validated[Violations, Option[B]] =
      self.decode(response).map(_.map(f))
    override def encode(b: B): Http.Response = self.encode(g(b))

  final def product[B](others: Headers[B]): Result[(A, B)] = new Result[(A, B)](description):
    export self.{body, code}
    override def headers: Headers[?] = self.headers.zip(others)
    override def decode(response: Http.Response): Validated[Violations, Option[(A, B)]] =
      self.decode(response).andThen(_.traverse(others.decode(response.headers).tupleLeft))
    override def encode(ab: (A, B)): Http.Response =
      self.encode(ab._1).modifyHeaders(_ ++ others.encode(ab._2))

  final def zip[B](headers: Headers[B])(using evidence: Evidence.Merge[A, B]): Result[evidence.Out] =
    product(headers).imap(evidence.apply)(evidence.unapply)

  final def :*[B](header: Header[B])(using evidence: Evidence.Merge[A, B]): Result[evidence.Out] =
    zip(header.toHeaders)

  final def orElse[B](result: Result[B]): Results[A + B] = toResults.orElse(result.toResults)
  def toResults: Results[A] = Results(this)

  def :+[B](result: Result[B]): Results[A + B] = orElse(result)

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Results[B] = toResults.to

  def decode(response: Http.Response): Validated[Violations, Option[A]]
  def encode(a: A): Http.Response

object Result:
  def apply[A](c: Code, b: Response.Body[A]): Result[A] = new Result[A](None):
    override def code: Code = c
    override def headers: Headers[Unit] = Headers.Empty
    override def body: Response.Body[A] = b
    override def decode(response: Http.Response): Validated[Violations, Option[A]] =
      if code =!= response.code
      then none.valid
      else body.decode(response.headers, response.body).map(_.some)
    override def encode(a: A): Http.Response =
      val (headers, payload) = body.encode(a)
      Http.Response(code, headers, payload)
