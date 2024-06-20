package io.taig.otter.validation

import cats.data.{Chain, Validated, ValidatedNec}
import cats.syntax.all.*
import cats.Applicative

sealed abstract class Validation[-In, +A, +B, +Out]:
  self =>
  def constraints: Chain[Constraint[A]]
  def apply(in: In): ValidatedNec[Violation[A, B], Out]

  def andThen[C, D, E](validation: Validation[Out, C, D, E]): Validation[In, A | C, B | D, E] = ???

//   final def first[C]: Validation[(In, C), (Out, C)] =
//     Validation(self.constraints) { case (a, c) => self(a).map((_, c)) }

object Validation:
  def apply[In, A, B, Out](cs: Chain[Constraint[A]])(
      f: In => ValidatedNec[Violation[A, B], Out]
  ): Validation[In, A, B, Out] = new Validation[In, A, B, Out]:
    override def constraints: Chain[Constraint[A]] = cs
    override def apply(in: In): ValidatedNec[Violation[A, B], Out] = f(in)

  def validated[In, A, B, Out](constraint: Constraint[A])(f: In => ValidatedNec[B, Out]): Validation[In, A, B, Out] =
    Validation(Chain.one(constraint))(f(_).leftMap(_.map(Violation(constraint, _))))

  def option[In, A, Out](constraint: Constraint[A])(f: In => Option[Out]): Validation[In, A, In, Out] =
    Validation(Chain.one(constraint))(in => f(in).toValidNec(Violation(constraint, in)))

  def when[In, A, Out](constraint: Constraint[A])(f: In => Boolean): Validation[In, A, In, Unit] =
    Validation(Chain.one(constraint))(in => Validated.condNec(f(in), (), Violation(constraint, in)))

  def lift[A, B](f: A => B): Validation[A, Nothing, Nothing, B] = Validation(Chain.empty)(f(_).valid)
  def valid[A](a: A): Validation[Any, Nothing, Nothing, A] = lift(_ => a)
  def ask[A]: Validation[A, Nothing, Nothing, A] = lift(identity)

  extension [In, A, B, Out](self: Validation[In, A, B, Out])
    def tap: Validation[In, A, B, In] = Validation(self.constraints)(a => self(a).as(a))

  given [In, A, B]: Applicative[Validation[In, A, B, *]] with
    override def pure[C](c: C): Validation[In, A, B, C] = valid(c)
    override def ap[C, D](ff: Validation[In, A, B, C => D])(fa: Validation[In, A, B, C]): Validation[In, A, B, D] =
      Validation(fa.constraints ++ ff.constraints)(a => (ff(a), fa(a)).mapN(_ apply _))

//   given Arrow[Validation] with
//     override def lift[A, B](f: A => B): Validation[A, B] = Validation.lift(f)
//     override def first[A, B, C](fa: Validation[A, B]): Validation[(A, C), (B, C)] = fa.first
//     override def compose[A, B, C](f: Validation[B, C], g: Validation[A, B]): Validation[A, C] =
//       Validation(g.constraints ++ f.constraints)(g(_).andThen(f.apply))
