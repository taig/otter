package io.taig.otter.validation

import cats.Applicative
import cats.arrow.Arrow
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.all.*

sealed abstract class Validation[+Act, -In, +Out]:
  self =>
  def constraints: Chain[Constraint]
  def apply(in: In): ValidatedNec[Violation[Act], Out]

  final def first[C]: Validation[Act, (In, C), (Out, C)] =
    Validation(self.constraints) { case (a, c) => self(a).map((_, c)) }

object Validation:
  def apply[Act, In, Out](cs: Chain[Constraint])(
      f: In => ValidatedNec[Violation[Act], Out]
  ): Validation[Act, In, Out] = new Validation[Act, In, Out]:
    override def constraints: Chain[Constraint] = cs
    override def apply(in: In): ValidatedNec[Violation[Act], Out] = f(in)

  def apply[Act, In, Out](constraint: Constraint)(f: In => ValidatedNec[Act, Out]): Validation[Act, In, Out] =
    Validation(Chain.one(constraint))(f(_).leftMap(_.map(act => Violation(constraint, act.some))))

  def lift[A, B](f: A => B): Validation[Nothing, A, B] = Validation(Chain.empty)(f(_).valid)
  def valid[A](a: A): Validation[Nothing, Any, A] = lift(_ => a)

  def parse[A](tpe: String)(f: String => Option[A]): Validation[String, String, A] =
    Validation(Constraint.Type(tpe)): value =>
      Validated.fromOption(f(value), NonEmptyChain.one(value))

  extension [Act, In, Out](self: Validation[Act, In, Out])
    def tap: Validation[Act, In, In] = Validation(self.constraints)(a => self(a).as(a))

  given [Act, In]: Applicative[Validation[Act, In, *]] with
    override def pure[B](b: B): Validation[Act, In, B] = valid(b)
    override def ap[B, C](ff: Validation[Act, In, B => C])(fa: Validation[Act, In, B]): Validation[Act, In, C] =
      Validation(fa.constraints ++ ff.constraints)(a => (ff(a), fa(a)).mapN(_ apply _))

  given [Act]: Arrow[Validation[Act, *, *]] with
    override def lift[A, B](f: A => B): Validation[Act, A, B] = Validation.lift(f)
    override def first[A, B, C](fa: Validation[Act, A, B]): Validation[Act, (A, C), (B, C)] = fa.first
    override def compose[A, B, C](f: Validation[Act, B, C], g: Validation[Act, A, B]): Validation[Act, A, C] =
      Validation(g.constraints ++ f.constraints)(g(_).andThen(f.apply))
