//package io.taig.otter.http
//
//import cats.data.{NonEmptyChain, Validated}
//import cats.syntax.all.*
//import io.taig.otter.schema.{+, Violations}
//
//sealed abstract class Results[A]:
//  self =>
//  def toNonEmptyChain: NonEmptyChain[Result[?]]
//
//  final def orElse[B](results: Results[B]): Results[A + B] = new Results[A + B]:
//    override def toNonEmptyChain: NonEmptyChain[Result[?]] = self.toNonEmptyChain.combine(results.toNonEmptyChain)
//    override def decode(response: Http.Response): Validated[Violations, A + B] = ???
//    override def encode(ab: A + B): Http.Response = ab match
//      case Left(a)  => self.encode(a)
//      case Right(b) => results.encode(b)
//
//  def decode(response: Http.Response): Validated[Violations, A]
//  def encode(a: A): Http.Response
//
//object Results:
//  def apply[A](result: Result[A]): Results[A] = new Results[A]:
//    override def toNonEmptyChain: NonEmptyChain[Result[?]] = NonEmptyChain.one(result)
//    override def decode(response: Http.Response): Validated[Violations, A] =
//      result.decode(response).andThen(_.toValid(???))
//    override def encode(a: A): Http.Response = result.encode(a)
