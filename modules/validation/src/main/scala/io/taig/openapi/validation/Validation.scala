package io.taig.openapi.validation

import cats.data.{Chain, ValidatedNec}
import cats.syntax.all.*

sealed abstract class Validation[+Act, -In, +Out]:
  self =>
  def constraints: Chain[Constraint]
  def apply(a: In): ValidatedNec[Violation[Act], Out]

//  final def first[C]: Validation[(In, C), (Out, C)] =
//    Validation(self.constraints) { case (a, c) => self(a).map(b => (b, c)) }
//  final def compose[C](fbc: Validation[Out, C]): Validation[In, C] =
//    Validation(self.constraints ++ fbc.constraints)(self(_).andThen(fbc.apply))

object Validation:
  def apply[Act, In, Out](constraints: Chain[Constraint])(
      f: In => ValidatedNec[Violation[Act], Out]
  ): Validation[Act, In, Out] =
    val c = constraints
    new Validation[Act, In, Out]:
      override def constraints: Chain[Constraint] = c
      override def apply(a: In): ValidatedNec[Violation[Act], Out] = f(a)

  def apply[Act, In, Out](constraint: Constraint)(f: In => ValidatedNec[Option[Act], Out]): Validation[Act, In, Out] =
    Validation(Chain.one(constraint))(f(_).leftMap(_.map(Violation(constraint, _))))

  def lift[A, B](f: A => B): Validation[Nothing, A, B] = Validation(Chain.empty)(f(_).valid)
//  def valid[A](a: A): Validation[Any, A] = lift(_ => a)
//
//  def parse[A](tpe: String)(f: String => Option[A]): Validation[String, A] =
//    Validation(Constraint.Type(tpe)): value =>
//      Validated.fromOption(f(value), NonEmptyChain.one(OpenApi.fromString(value).some))
//
//  extension [A, B](self: Validation[A, B]) def tap: Validation[A, A] = Validation(self.constraints)(a => self(a).as(a))
//
//  given [A]: Applicative[Validation[A, *]] = new Applicative[Validation[A, *]]:
//    override def pure[B](b: B): Validation[A, B] = valid(b)
//    override def ap[B, C](ff: Validation[A, B => C])(fa: Validation[A, B]): Validation[A, C] =
//      Validation(fa.constraints ++ ff.constraints)(a => (ff(a), fa(a)).mapN(_ apply _))
//
//  given Arrow[Validation] with
//    override def lift[A, B](f: A => B): Validation[A, B] = Validation.lift(f)
//    override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] = fa.first
//    override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] = g.compose(f)
