package io.taig.otter.http

import cats.data.{NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.otter.{+, Evidence}
import io.taig.otter.validation.Violations

sealed abstract class Results[A]:
  self =>
  def toNonEmptyChain: NonEmptyChain[Result[?]]

  final def imap[B](f: A => B)(g: B => A): Results[B] = new Results[B]:
    export self.toNonEmptyChain
    override def decode(response: Http.Response): Validated[Violations, B] = self.decode(response).map(f)
    override def encode(b: B): Http.Response = self.encode(g(b))

  final def orElse[B](results: Results[B]): Results[A + B] = new Results[A + B]:
    override def toNonEmptyChain: NonEmptyChain[Result[?]] = self.toNonEmptyChain.combine(results.toNonEmptyChain)
    override def decode(response: Http.Response): Validated[Violations, A + B] = ???
    override def encode(ab: A + B): Http.Response = ab match
      case Left(a)  => self.encode(a)
      case Right(b) => results.encode(b)

  final def :+[B](result: Result[B]): Results[A + B] = orElse(result.toResults)
  final def +:[B](result: Result[B]): Results[B + A] = result.toResults.orElse(this)

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Results[B] = imap(evidence.from)(evidence.to)

  def decode(response: Http.Response): Validated[Violations, A]
  def encode(a: A): Http.Response

object Results:
  def apply[A](result: Result[A]): Results[A] = new Results[A]:
    override def toNonEmptyChain: NonEmptyChain[Result[?]] = NonEmptyChain.one(result)
    override def decode(response: Http.Response): Validated[Violations, A] =
      result.decode(response).andThen(_.toValid(???))
    override def encode(a: A): Http.Response = result.encode(a)
