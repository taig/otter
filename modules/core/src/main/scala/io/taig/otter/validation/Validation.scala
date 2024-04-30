package io.taig.otter.validation

import cats.Applicative
import cats.arrow.Arrow
import cats.data.{Chain, Validated, ValidatedNec}
import cats.syntax.all.*

sealed abstract class Validation[-In, +C, +Out]:
  self =>
  def constraints: Chain[Constraint[C]]
  def apply(in: In): ValidatedNec[Violation[C], Out]

//   final def first[C]: Validation[(In, C), (Out, C)] =
//     Validation(self.constraints) { case (a, c) => self(a).map((_, c)) }

object Validation:
  def apply[In, C, Out](cs: Chain[Constraint[C]])(
      f: In => ValidatedNec[Violation[C], Out]
  ): Validation[In, C, Out] = new Validation[In, C, Out]:
    override def constraints: Chain[Constraint[C]] = cs
    override def apply(in: In): ValidatedNec[Violation[C], Out] = f(in)

  def of[In, C, Out](constraint: Constraint[In])(
      f: In => ValidatedNec[In, Out]
  ): Validation[In, In, Out] = Validation(Chain.one(constraint))(f(_).leftMap(_.map(Violation(constraint, _))))

  def lift[A, B](f: A => B): Validation[A, Nothing, B] = Validation(Chain.empty)(f(_).valid)
  def valid[A](a: A): Validation[Any, Nothing, A] = lift(_ => a)
  def ask[A]: Validation[A, Nothing, A] = Validation(Chain.empty)(_.valid)

  extension [In, C, Out](self: Validation[In, C, Out])
    def tap: Validation[In, C, In] = Validation(self.constraints)(a => self(a).as(a))

//   given [In]: Applicative[Validation[In, *]] with
//     override def pure[B](b: B): Validation[In, B] = valid(b)
//     override def ap[B, C](ff: Validation[In, B => C])(fa: Validation[In, B]): Validation[In, C] =
//       Validation(fa.constraints ++ ff.constraints)(a => (ff(a), fa(a)).mapN(_ apply _))

//   given Arrow[Validation] with
//     override def lift[A, B](f: A => B): Validation[A, B] = Validation.lift(f)
//     override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] = fa.first
//     override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] =
//       Validation(g.constraints ++ f.constraints)(g(_).andThen(f.apply))
