package io.taig.otter.http

import cats.data.Ior
import cats.data.NonEmptyVector
import cats.syntax.all.*
import io.taig.otter.Convert
import io.taig.otter.Violations
import io.taig.otter.http.header.Accept

import java.nio.charset.Charset

sealed abstract class Results[A]:
  self =>
  def toNev: NonEmptyVector[Result[?]]

  final def imap[B](f: A => B)(g: B => A): Results[B] = new Results[B]:
    export self.toNev
    override def decode(response: Http.Response): Ior[Violations, Option[B]] =
      self.decode(response).map(_.map(f))
    override def encode(accept: Accept.Result, b: B): Option[Http.Response] = self.encode(accept, g(b))
    override def encode(charset: Option[Charset], b: B): Http.Response = self.encode(charset, g(b))

  final infix def orElse[B](results: Results[B]): Results[Either[A, B]] = new Results[Either[A, B]]:
    override def toNev: NonEmptyVector[Result[?]] = self.toNev.concatNev(results.toNev)
    override def decode(response: Http.Response): Ior[Violations, Option[Either[A, B]]] = self.decode(response) match
      case Ior.Right(Some(a)) => a.asLeft.some.rightIor
      case Ior.Right(None)    => results.decode(response).map(_.map(_.asRight))
      case Ior.Left(left)     =>
        results.decode(response) match
          case Ior.Right(b)       => Ior.Both(left, b.map(_.asRight))
          case Ior.Left(right)    => Ior.Left(left.combine(right))
          case Ior.Both(right, b) => Ior.Both(left.combine(right), b.map(_.asRight))
      case Ior.Both(left, Some(a)) => Ior.Both(left, a.asLeft.some)
      case Ior.Both(left, None)    =>
        results.decode(response) match
          case Ior.Right(b)       => Ior.Both(left, b.map(_.asRight))
          case Ior.Left(right)    => Ior.Both(left.combine(right), none)
          case Ior.Both(right, b) => Ior.Both(left.combine(right), b.map(_.asRight))
    override def encode(accept: Accept.Result, ab: Either[A, B]): Option[Http.Response] = ab match
      case Left(a)  => self.encode(accept, a)
      case Right(b) => results.encode(accept, b)
    override def encode(charset: Option[Charset], a: Either[A, B]): Http.Response =
      a.fold(self.encode(charset, _), results.encode(charset, _))

  final def :+[B](result: Result[B]): Results[Either[A, B]] = orElse(result.toResults)
  final def +:[B](result: Result[B]): Results[Either[B, A]] = result.toResults.orElse(this)

  final def to[B](using convert: Convert[A, B]): Results[B] = imap(convert.to)(convert.from)

  def decode(response: Http.Response): Ior[Violations, Option[A]]

  def encode(accept: Accept.Result, a: A): Option[Http.Response]

  def encode(charset: Option[Charset], a: A): Http.Response

object Results:
  extension [A <: Matchable](self: Results[A])
    inline def |[B <: Matchable](result: Result[B]): Results[A | B] = (self :+ result).imap {
      case Left(a)  => a
      case Right(b) => b
    } {
      case a: A => Left(a)
      case b: B => Right(b)
    }

  def apply[A](result: Result[A]): Results[A] = new Results[A]:
    override def toNev: NonEmptyVector[Result[?]] = NonEmptyVector.one(result)
    override def decode(response: Http.Response): Ior[Violations, Option[A]] =
      result.decode(response).toIor
    override def encode(accept: Accept.Result, a: A): Option[Http.Response] = result.encode(accept, a)
    override def encode(charset: Option[Charset], a: A): Http.Response = result.encode(charset, a)
