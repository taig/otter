package io.taig.validation

import cats.arrow.Arrow
import cats.data.*
import cats.syntax.all.*
import cats.{Applicative, Eq, Foldable, Traverse, UnorderedFoldable}
import io.taig.validation.Validation.valid

sealed abstract class Validation[+Ref, +Act, -In, +Out]:
  def constraints: Chain[Constraint[Ref]]

  final def modifyConstraints[Ref2](f: Constraint[Ref] => Constraint[Ref2]): Validation[Ref2, Act, In, Out] =
    Validation(constraints.map(f))(run(_).leftMap(_.map(_.modifyConstraint(f))))

  final def modifyConstraint[Ref2](
      pf: PartialFunction[Constraint[Ref], Constraint[Ref2]]
  ): Validation[Ref | Ref2, Act, In, Out] = Validation(constraints.mapFilter(pf.lift)) { input =>
    run(input).leftMap(_.map(_.modifyConstraint { constraint =>
      if pf.isDefinedAt(constraint) then pf.apply(constraint) else constraint
    }))
  }

  final def withConstraint[Ref2](f: Option[Ref] => Constraint[Ref2]): Validation[Ref2, Act, In, Out] =
    modifyConstraints(constraint => f(constraint.toRules.collectFirstSome(_.reference)))

  final def map[B](f: Out => B): Validation[Ref, Act, In, B] = Validation(constraints)(run(_).map(f))

  final def mapReference[B](f: Ref => B): Validation[B, Act, In, Out] =
    Validation(constraints.map(_.map(f)))(run(_).leftMap(_.map(_.mapConstraint(f))))

  final def contramap[In2](f: In2 => In): Validation[Ref, Act, In2, Out] =
    Validation(constraints)(input => run(f(input)))

  def run(input: In): ValidatedNec[Violation[Ref, Act], Out]

  final def andThen[Ref2, Act2, Out2](
      validation: => Validation[Ref2, Act2, Out, Out2]
  ): Validation[Ref | Ref2, Act | Act2, In, Out2] =
    Validation(constraints ++ validation.constraints)(run(_).andThen(validation.run))

  final def product[Ref2 >: Ref, Act2 >: Act, In2 <: In, Out2](
      validation: Validation[Ref2, Act2, In2, Out2]
  ): Validation[Ref2, Act2, In2, (Out, Out2)] = Validation(constraints ++ validation.constraints) { input =>
    run(input).product(validation.run(input))
  }
  final def <*[Ref2 >: Ref, Act2 >: Act, In2 <: In, Out2](
      validation: Validation[Ref2, Act2, In2, Out2]
  ): Validation[Ref2, Act2, In2, Out] = product(validation).map(_._1)
  final def *>[Ref2 >: Ref, Act2 >: Act, In2 <: In, Out2](
      validation: Validation[Ref2, Act2, In2, Out2]
  ): Validation[Ref2, Act2, In2, Out2] = product(validation).map(_._2)

object Validation:
  extension [Ref, Act, In, Out](self: Validation[Ref, Act, In, Out])

    /** Reset the `actual` value of a `Violation` to the initial input */
    def reset: Validation[Ref, In, In, Out] = Validation(self.constraints) { input =>
      self.run(input).leftMap(_.map(_.withActual(input)))
    }

    /** Return the input of this validation as the output */
    def tap: Validation[Ref, Act, In, In] = Validation(self.constraints)(input => self.run(input).as(input))

    infix def orElse[Ref2, Out2](
        validation: Validation[Ref2, Act, In, Out2]
    ): Validation[Ref | Ref2, Act, In, Either[Out, Out2]] =
      Validation(Chain.one(Constraint.Or(self.constraints, validation.constraints))) { input =>
        self.run(input).map(_.asLeft[Out2]) match
          case Validated.Valid(out) => out.valid
          case Validated.Invalid(left) =>
            validation.run(input).map(_.asRight[Out]) match
              case Validated.Valid(out2)    => out2.valid
              case Validated.Invalid(right) => (left.concat(right)).invalid
      }

    def modifyViolations[Act2](
        f: NonEmptyChain[Violation[Ref, Act]] => NonEmptyChain[Violation[Ref, Act2]]
    ): Validation[Ref, Act2, In, Out] = Validation(self.constraints)(input => self.run(input).leftMap(f))

    def modifyViolation[Act2](f: Violation[Ref, Act] => Violation[Ref, Act2]): Validation[Ref, Act2, In, Out] =
      modifyViolations(_.map(f))

    def mapActual[Act2](f: Act => Act2): Validation[Ref, Act2, In, Out] = modifyViolation(_.mapActual(f))

  private def apply[Ref, Act, In, Out](c: => Chain[Constraint[Ref]])(
      f: In => ValidatedNec[Violation[Ref, Act], Out]
  ): Validation[Ref, Act, In, Out] = new Validation[Ref, Act, In, Out]:
    final override def constraints: Chain[Constraint[Ref]] = c
    final override def run(input: In): ValidatedNec[Violation[Ref, Act], Out] = f(input)

  def invalid[Ref, Act](violations: NonEmptyChain[Violation[Ref, Act]]): Validation[Ref, Act, Any, Nothing] =
    Validation(violations.map(_.constraint).toChain)(_ => Validated.invalid(violations))

  def invalidNec[Ref, Act](violation: Violation[Ref, Act]): Validation[Ref, Act, Any, Nothing] =
    invalid(NonEmptyChain.one(violation))

  def valid[Out](output: => Out): Validation[Nothing, Nothing, Any, Out] =
    Validation(Chain.empty)(_ => Validated.validNec(output))

  def ask[A]: Validation[Nothing, Nothing, A, A] = Validation(Chain.empty)(Validated.valid)

  def fromFunction[A, B](f: A => B): Validation[Nothing, Nothing, A, B] = Validation(Chain.empty)(f(_).valid)

  def cond[Ref, In](constraints: NonEmptyChain[Constraint[Ref]])(f: In => Boolean): Validation[Ref, In, In, Unit] =
    Validation(constraints.toChain) { input =>
      Validated.cond(f(input), (), constraints.map(Violation(_, input)))
    }

  def condNec[Ref, In](constraint: Constraint[Ref])(f: In => Boolean): Validation[Ref, In, In, Unit] =
    cond(NonEmptyChain.one(constraint))(f)

  def fromOption[Ref, In, Out](constraints: NonEmptyChain[Constraint[Ref]])(
      f: In => Option[Out]
  ): Validation[Ref, In, In, Out] = Validation(constraints.toChain) { input =>
    Validated.fromOption(f(input), constraints.map(Violation(_, input)))
  }

  def fromOptionNec[Ref, In, Out](constraint: Constraint[Ref])(f: In => Option[Out]): Validation[Ref, In, In, Out] =
    fromOption(NonEmptyChain.one(constraint))(f)

  def collect[A, B, C](constraints: NonEmptyChain[Constraint[A]])(pf: PartialFunction[B, C]): Validation[A, B, B, C] =
    Validation.fromOption(constraints)(pf.lift)

  def collectNec[A, B, C](constraint: Constraint[A])(pf: PartialFunction[B, C]): Validation[A, B, B, C] =
    collect(NonEmptyChain.one(constraint))(pf)

  given [Ref, Act, In]: Applicative[Validation[Ref, Act, In, *]] with
    override def pure[A](x: A): Validation[Ref, Act, In, A] = valid(x)
    override def product[A, B](
        fa: Validation[Ref, Act, In, A],
        fb: Validation[Ref, Act, In, B]
    ): Validation[Ref, Act, In, (A, B)] = fa.product(fb)
    override def ap[A, B](ff: Validation[Ref, Act, In, A => B])(
        fa: Validation[Ref, Act, In, A]
    ): Validation[Ref, Act, In, B] = Validation(ff.constraints ++ fa.constraints) { input =>
      (ff.run(input), fa.run(input)).mapN(_ apply _)
    }

  given [Ref, Act]: Arrow[Validation[Ref, Act, *, *]] with
    override def lift[A, B](f: A => B): Validation[Ref, Act, A, B] = fromFunction(f)
    override def compose[A, B, C](
        f: Validation[Ref, Act, B, C],
        g: Validation[Ref, Act, A, B]
    ): Validation[Ref, Act, A, C] = g.andThen(f)
    override def first[A, B, C](fa: Validation[Ref, Act, A, B]): Validation[Ref, Act, (A, C), (B, C)] =
      Validation(fa.constraints) { case (a, c) => fa.run(a).tupleRight(c) }
