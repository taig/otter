package io.taig.otter.validation

import cats.Applicative
import cats.arrow.Arrow
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.all.*
import io.taig.otter.OpenApi

sealed abstract class Validation[-In, +Out]:
  self =>
  def constraints: Chain[Constraint]
  def apply(in: In): ValidatedNec[Violation, Out]

  final def first[C]: Validation[(In, C), (Out, C)] =
    Validation(self.constraints) { case (a, c) => self(a).map((_, c)) }

object Validation:
  def apply[In, Out](cs: Chain[Constraint])(
      f: In => ValidatedNec[Violation, Out]
  ): Validation[In, Out] = new Validation[In, Out]:
    override def constraints: Chain[Constraint] = cs
    override def apply(in: In): ValidatedNec[Violation, Out] = f(in)

  def apply[In, Out](constraint: Constraint)(f: In => ValidatedNec[OpenApi, Out]): Validation[In, Out] =
    Validation(Chain.one(constraint))(f(_).leftMap(_.map(Violation(constraint, _))))

  def lift[A, B](f: A => B): Validation[A, B] = Validation(Chain.empty)(f(_).valid)
  def valid[A](a: A): Validation[Any, A] = lift(_ => a)

  def parse[A](tpe: String)(f: String => Option[A]): Validation[String, A] =
    Validation(Constraint.Type(tpe)): value =>
      Validated.fromOption(f(value), NonEmptyChain.one(OpenApi.String(value)))

  extension [In, Out](self: Validation[In, Out])
    def tap: Validation[In, In] =
      Validation(self.constraints)(a => self(a).as(a))

  given [In]: Applicative[Validation[In, *]] with
    override def pure[B](b: B): Validation[In, B] = valid(b)
    override def ap[B, C](ff: Validation[In, B => C])(fa: Validation[In, B]): Validation[In, C] =
      Validation(fa.constraints ++ ff.constraints)(a => (ff(a), fa(a)).mapN(_ apply _))

  given [Act]: Arrow[Validation] with
    override def lift[A, B](f: A => B): Validation[A, B] = Validation.lift(f)
    override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] = fa.first
    override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] =
      Validation(g.constraints ++ f.constraints)(g(_).andThen(f.apply))
