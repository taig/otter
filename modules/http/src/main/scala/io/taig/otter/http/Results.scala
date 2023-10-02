package io.taig.otter.http

import cats.data.{NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.{+, Evidence}

sealed abstract class Results[A]:
  self =>
  def toNonEmptyChain: NonEmptyChain[Result[?]]

  final def imap[B](f: A => B)(g: B => A): Results[B] = new Results[B]:
    export self.toNonEmptyChain
    override def decodeOption(response: Http.Response): Validated[Violations, Option[B]] =
      self.decodeOption(response).map(_.map(f))
    override def encode(b: B): Http.Response = self.encode(g(b))

  final def orElse[B](results: Results[B]): Results[A + B] = new Results[A + B]:
    override def toNonEmptyChain: NonEmptyChain[Result[?]] = self.toNonEmptyChain.combine(results.toNonEmptyChain)
    // TODO Ior (?)
    override def decodeOption(response: Http.Response): Validated[Violations, Option[A + B]] =
      self.decodeOption(response) match
        case Validated.Valid(Some(a)) => a.asLeft.some.valid
        case Validated.Valid(None)    => results.decodeOption(response).map(_.map(_.asRight))
        case Validated.Invalid(left)  => results.decodeOption(response).map(_.map(_.asRight)).leftMap(left |+| _)
    override def encode(ab: A + B): Http.Response = ab match
      case Left(a)  => self.encode(a)
      case Right(b) => results.encode(b)

  final def :+[B](result: Result[B]): Results[A + B] = orElse(result.toResults)
  final def +:[B](result: Result[B]): Results[B + A] = result.toResults.orElse(this)

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Results[B] = imap(evidence.from)(evidence.to)

  final def decode(response: Http.Response): Validated[Violations, A] =
    decodeOption(response).andThen(_.toValid(???))
  protected def decodeOption(response: Http.Response): Validated[Violations, Option[A]]
  def encode(a: A): Http.Response

object Results:
  def apply[A](result: Result[A]): Results[A] = new Results[A]:
    override def toNonEmptyChain: NonEmptyChain[Result[?]] = NonEmptyChain.one(result)
    override def decodeOption(response: Http.Response): Validated[Violations, Option[A]] = result.decode(response)
    override def encode(a: A): Http.Response = result.encode(a)
