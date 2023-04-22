package io.taig.validation

import cats.data.*
import cats.syntax.all.*

sealed abstract class Validation[-I, +O]:
  def constraints: Chain[Constraint]

  final def contramap[I2](f: I2 => I): Validation[I2, O] = Validation(constraints)(input => run(f(input)))

  final def map[O2](f: O => O2): Validation[I, O2] = Validation(constraints)(run(_).map(f))

  final def andThen[O2](validation: => Validation[O, O2]): Validation[I, O2] =
    Validation(constraints ++ validation.constraints)(run(_).andThen(validation.run))

  final def product[I2 <: I, O2](validation: Validation[I2, O2]): Validation[I2, (O, O2)] =
    Validation(constraints ++ validation.constraints)(input => run(input).product(validation.run(input)))

  final def <*[I2 <: I, O2](validation: Validation[I2, O2]): Validation[I2, O] = product(validation).map(_._1)

  final def *>[I2 <: I, O2](validation: Validation[I2, O2]): Validation[I2, O2] = product(validation).map(_._2)

  def run(input: I): ValidatedNec[Violation, O]

object Validation:
  extension [I, O](self: Validation[I, O])
    /** Return the input of this validation as the output */
    def tap: Validation[I, I] = Validation(self.constraints)(input => self.run(input).as(input))

  def apply[I, O](c: => Chain[Constraint])(f: I => ValidatedNec[Violation, O]): Validation[I, O] = new Validation[I, O]:
    final override def constraints: Chain[Constraint] = c
    final override def run(input: I): ValidatedNec[Violation, O] = f(input)

  def cond[I](constraint: Constraint)(f: I => Boolean): Validation[I, Unit] =
    Validation(Chain.one(constraint)): input =>
      Validated.cond(f(input), (), NonEmptyChain.one(Violation(constraint, ???)))

//object Validation:

//
//    infix def orElse[Ref2, Out2](
//        validation: Validation[Ref2, Act, In, Out2]
//    ): Validation[Ref | Ref2, Act, In, Either[Out, Out2]] =
//      Validation(Chain.one(Constraint.Or(self.constraints, validation.constraints))) { input =>
//        self.run(input).map(_.asLeft[Out2]) match
//          case Validated.Valid(out) => out.valid
//          case Validated.Invalid(left) =>
//            validation.run(input).map(_.asRight[Out]) match
//              case Validated.Valid(out2)    => out2.valid
//              case Validated.Invalid(right) => (left.concat(right)).invalid
//      }
//
//    def modifyViolations[Act2](
//        f: NonEmptyChain[Violation[Ref, Act]] => NonEmptyChain[Violation[Ref, Act2]]
//    ): Validation[Ref, Act2, In, Out] = Validation(self.constraints)(input => self.run(input).leftMap(f))
//
//    def modifyViolation[Act2](f: Violation[Ref, Act] => Violation[Ref, Act2]): Validation[Ref, Act2, In, Out] =
//      modifyViolations(_.map(f))
//
//    def mapActual[Act2](f: Act => Act2): Validation[Ref, Act2, In, Out] = modifyViolation(_.mapActual(f))
//
//  def invalid[Ref, Act](violations: NonEmptyChain[Violation[Ref, Act]]): Validation[Ref, Act, Any, Nothing] =
//    Validation(violations.map(_.constraint).toChain)(_ => Validated.invalid(violations))
//
//  def invalidNec[Ref, Act](violation: Violation[Ref, Act]): Validation[Ref, Act, Any, Nothing] =
//    invalid(NonEmptyChain.one(violation))
//
//  def valid[Out](output: => Out): Validation[Nothing, Nothing, Any, Out] =
//    Validation(Chain.empty)(_ => Validated.validNec(output))
//
//  def ask[A]: Validation[Nothing, Nothing, A, A] = Validation(Chain.empty)(Validated.valid)
//
//  def fromFunction[A, B](f: A => B): Validation[Nothing, Nothing, A, B] = Validation(Chain.empty)(f(_).valid)
//
//  def cond[Ref, In](constraints: NonEmptyChain[Constraint[Ref]])(f: In => Boolean): Validation[Ref, In, In, Unit] =
//    Validation(constraints.toChain) { input =>
//      Validated.cond(f(input), (), constraints.map(Violation(_, input)))
//    }
//
//  def condNec[Ref, In](constraint: Constraint[Ref])(f: In => Boolean): Validation[Ref, In, In, Unit] =
//    cond(NonEmptyChain.one(constraint))(f)
//
//  def fromOption[Ref, In, Out](constraints: NonEmptyChain[Constraint[Ref]])(
//      f: In => Option[Out]
//  ): Validation[Ref, In, In, Out] = Validation(constraints.toChain) { input =>
//    Validated.fromOption(f(input), constraints.map(Violation(_, input)))
//  }
//
//  def fromOptionNec[Ref, In, Out](constraint: Constraint[Ref])(f: In => Option[Out]): Validation[Ref, In, In, Out] =
//    fromOption(NonEmptyChain.one(constraint))(f)
//
//  def collect[A, B, C](constraints: NonEmptyChain[Constraint[A]])(pf: PartialFunction[B, C]): Validation[A, B, B, C] =
//    Validation.fromOption(constraints)(pf.lift)
//
//  def collectNec[A, B, C](constraint: Constraint[A])(pf: PartialFunction[B, C]): Validation[A, B, B, C] =
//    collect(NonEmptyChain.one(constraint))(pf)
//
//  given [Ref, Act, In]: Applicative[Validation[Ref, Act, In, *]] with
//    override def pure[A](x: A): Validation[Ref, Act, In, A] = valid(x)
//    override def product[A, B](
//        fa: Validation[Ref, Act, In, A],
//        fb: Validation[Ref, Act, In, B]
//    ): Validation[Ref, Act, In, (A, B)] = fa.product(fb)
//    override def ap[A, B](ff: Validation[Ref, Act, In, A => B])(
//        fa: Validation[Ref, Act, In, A]
//    ): Validation[Ref, Act, In, B] = Validation(ff.constraints ++ fa.constraints) { input =>
//      (ff.run(input), fa.run(input)).mapN(_ apply _)
//    }
//
//  given [Ref, Act]: Arrow[Validation[Ref, Act, *, *]] with
//    override def lift[A, B](f: A => B): Validation[Ref, Act, A, B] = fromFunction(f)
//    override def compose[A, B, C](
//        f: Validation[Ref, Act, B, C],
//        g: Validation[Ref, Act, A, B]
//    ): Validation[Ref, Act, A, C] = g.andThen(f)
//    override def first[A, B, C](fa: Validation[Ref, Act, A, B]): Validation[Ref, Act, (A, C), (B, C)] =
//      Validation(fa.constraints) { case (a, c) => fa.run(a).tupleRight(c) }
