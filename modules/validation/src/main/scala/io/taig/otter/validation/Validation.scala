package io.taig.otter.validation

import cats.data.{Chain, Validated, ValidatedNec}
import cats.syntax.all.*
import cats.Applicative
import cats.arrow.Arrow

sealed abstract class Validation[-In, +A, +B, +Out]:
  def constraints: Chain[A]
  def apply(in: In): ValidatedNec[Violation[A, B], Out]

  def andThen[C, D, E](validation: Validation[Out, C, D, E]): Validation[In, A | C, B | D, E] =
    Validation(constraints ++ validation.constraints)(apply(_).andThen(validation.apply))

  def mapConstraint[C](f: A => C): Validation[In, C, B, Out] =
    Validation(constraints.map(f))(apply(_).leftMap(_.map(_.leftMap(f))))

  def mapActual[C](f: B => C): Validation[In, A, C, Out] =
    Validation(constraints)(apply(_).leftMap(_.map(_.map(f))))

  final def first[C]: Validation[(In, C), A, B, (Out, C)] =
    Validation(constraints)(apply(_).tupleRight(_))

  final def tap[In1 <: In]: Validation[In1, A, B, In1] = Validation(constraints)(a => apply(a).as(a))

object Validation:
  def apply[In, A, B, Out](constraints: Chain[A])(
      f: In => ValidatedNec[Violation[A, B], Out]
  ): Validation[In, A, B, Out] =
    val _constraints = constraints

    new Validation[In, A, B, Out]:
      override def constraints: Chain[A] = _constraints
      override def apply(in: In): ValidatedNec[Violation[A, B], Out] = f(in)

  def validated[In, A, B, Out](constraint: A)(f: In => ValidatedNec[B, Out]): Validation[In, A, B, Out] =
    Validation(Chain.one(constraint))(f(_).leftMap(_.map(Violation(constraint, _))))

  def option[In, A, Out](constraint: A)(f: In => Option[Out]): Validation[In, A, In, Out] =
    Validation(Chain.one(constraint))(in => f(in).toValidNec(Violation(constraint, in)))

  def when[In, A](constraint: A)(f: In => Boolean): Validation[In, A, In, Unit] =
    Validation(Chain.one(constraint))(in => Validated.condNec(f(in), (), Violation(constraint, in)))

  def lift[A, B](f: A => B): Validation[A, Nothing, Nothing, B] = Validation(Chain.empty)(f(_).valid)
  def valid[A](a: A): Validation[Any, Nothing, Nothing, A] = lift(_ => a)
  def ask[A]: Validation[A, Nothing, Nothing, A] = lift(identity)

  given [In, A, B]: Applicative[Validation[In, A, B, *]] with
    override def pure[C](c: C): Validation[In, A, B, C] = valid(c)
    override def ap[C, D](ff: Validation[In, A, B, C => D])(fa: Validation[In, A, B, C]): Validation[In, A, B, D] =
      Validation(fa.constraints ++ ff.constraints)(a => (ff(a), fa(a)).mapN(_ apply _))

  given [A, B]: Arrow[Validation[*, A, B, *]] with
    override def lift[C, D](f: C => D): Validation[C, A, B, D] = Validation.lift(f)
    override def first[C, D, E](fa: Validation[C, A, B, D]): Validation[(C, E), A, B, (D, E)] = fa.first
    override def compose[C, D, E](f: Validation[D, A, B, E], g: Validation[C, A, B, D]): Validation[C, A, B, E] =
      Validation(g.constraints ++ f.constraints)(g(_).andThen(f.apply))
