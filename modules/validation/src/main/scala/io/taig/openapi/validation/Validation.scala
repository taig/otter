package io.taig.openapi.validation

import cats.Applicative
import cats.arrow.Arrow
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.all.*
import io.taig.openapi.OpenApi

sealed abstract class Validation[-A, +B]:
  self =>
  def constraints: Chain[Constraint]
  def apply(a: A): ValidatedNec[Violation, B]

  final def first[C]: Validation[(A, C), (B, C)] =
    Validation(self.constraints) { case (a, c) => self(a).map(b => (b, c)) }
  final def compose[C](fbc: Validation[B, C]): Validation[A, C] =
    Validation(self.constraints ++ fbc.constraints)(self(_).andThen(fbc.apply))

object Validation:
  def apply[A, B](constraints: Chain[Constraint])(f: A => ValidatedNec[Violation, B]): Validation[A, B] =
    val c = constraints
    new Validation[A, B]:
      override def constraints: Chain[Constraint] = c
      override def apply(a: A): ValidatedNec[Violation, B] = f(a)

  def apply[A, B](constraint: Constraint)(f: A => ValidatedNec[Option[OpenApi], B]): Validation[A, B] =
    Validation(Chain.one(constraint))(a => f(a).leftMap(_.map(Violation(constraint, _))))

  def lift[A, B](f: A => B): Validation[A, B] = Validation(Chain.empty)(f(_).valid)
  def valid[A](a: A): Validation[Any, A] = lift(_ => a)

  def parse[A](tpe: String)(f: String => Option[A]): Validation[String, A] =
    Validation(Constraint.Type(tpe)): value =>
      Validated.fromOption(f(value), NonEmptyChain.one(OpenApi.fromString(value).some))

  extension [A, B](self: Validation[A, B]) def tap: Validation[A, A] = Validation(self.constraints)(a => self(a).as(a))

  given [A]: Applicative[Validation[A, *]] = new Applicative[Validation[A, *]]:
    override def pure[B](b: B): Validation[A, B] = valid(b)
    override def ap[B, C](ff: Validation[A, B => C])(fa: Validation[A, B]): Validation[A, C] =
      Validation(fa.constraints ++ ff.constraints)(a => (ff(a), fa(a)).mapN(_ apply _))

  given Arrow[Validation] with
    override def lift[A, B](f: A => B): Validation[A, B] = Validation.lift(f)
    override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] = fa.first
    override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] = g.compose(f)
