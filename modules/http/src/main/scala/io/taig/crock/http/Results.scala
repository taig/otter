//package io.taig.crock.http
//
//import cats.Invariant
//import cats.data.{NonEmptyChain, Validated}
//import cats.syntax.all.*
//
//sealed abstract class Results[A]:
//  def toNonEmptyChain: NonEmptyChain[Result[?]]
//  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Results[C] =
//    Results.Validate(this, validation, g)
//  final def imap[B](f: A => B)(g: B => A): Results[B] = ivalidate(Validation.lift(f))(g)
//  final def orElse[B](results: Results[B]): Results[A + B] = Results.OrElse(this, results)
//  final def :+[B](result: Result[B]): Results[A + B] = orElse(result.toResults)
//  final def +:[B](result: Result[B]): Results[B + A] = result.toResults.orElse(this)
//  def decode(response: Response): Validated[Violations, Option[A]]
//  def encode(a: A): Response
//
//object Results:
//  final private case class Root[A](result: Result[A]) extends Results[A]:
//    override def toNonEmptyChain: NonEmptyChain[Result[?]] = NonEmptyChain.one(result)
//    override def decode(response: Response): Validated[Violations, Option[A]] =
//      if result.code === response.code then result.decode(response).map(_.some) else none[A].valid
//    override def encode(a: A): Response = result.encode(a)
//
//  final private case class OrElse[A, B](left: Results[A], right: Results[B]) extends Results[A + B]:
//    override def toNonEmptyChain: NonEmptyChain[Result[?]] = left.toNonEmptyChain ++ right.toNonEmptyChain
//    // TODO is this right?
//    override def decode(response: Response): Validated[Violations, Option[A + B]] = left
//      .decode(response)
//      .andThen {
//        case Some(a) => a.asLeft.some.valid
//        case None    => right.decode(response).map(_.map(_.asRight))
//      }
//      .findValid(right.decode(response).map(_.map(_.asRight)))
//    override def encode(ab: A + B): Response = ab.fold(left.encode, right.encode)
//
//  final private case class Validate[A, B: Encoder, C](
//      results: Results[A],
//      validation: Validation[B, A, A, C],
//      g: C => A
//  ) extends Results[C]:
//    export results.toNonEmptyChain
//    override def decode(response: Response): Validated[Violations, Option[C]] =
//      results.decode(response).andThen(_.traverse(applyValidation(validation, results.encode(_).asOpenApi)))
//    override def encode(c: C): Response = results.encode(g(c))
//
//  def apply[A](result: Result[A]): Results[A] = Root(result)
//
//  given Invariant[Results] with
//    override def imap[A, B](fa: Results[A])(f: A => B)(g: B => A): Results[B] = fa.imap(f)(g)
