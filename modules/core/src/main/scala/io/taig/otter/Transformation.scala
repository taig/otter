package io.taig.otter

import io.taig.otter.validation.Validation
import cats.syntax.all.*

trait Transformation[A, +B, +C, D]:
  def validation: Validation[A, B, C, D]

  def apply(d: D): A

  final def imap[E](f: D => E)(g: E => D): Transformation[A, B, C, E] =
    Transformation(validation.map(f))(g.andThen(apply))

  final def ivalidate[E, F, G](validation: Validation[D, E, F, G])(f: G => D): Transformation[A, B | E, C | F, G] =
    Transformation(this.validation.andThen(validation))(f.andThen(apply))

  final def ivalidate_[E, F](validation: Validation[D, E, F, Unit]): Transformation[A, B | E, C | F, D] =
    ivalidate(validation.tap)(identity)

  final def mapValidation[E, F](f: Validation[A, B, C, D] => Validation[A, E, F, D]): Transformation[A, E, F, D] =
    Transformation(f(validation))(apply)

object Transformation:
  type Plain[A, D] = Transformation[A, Nothing, Nothing, D]

  def apply[A, B, C, D](validation: Validation[A, B, C, D])(f: D => A): Transformation[A, B, C, D] =
    val _validation = validation
    new Transformation[A, B, C, D]:
      override def validation: Validation[A, B, C, D] = _validation
      override def apply(d: D): A = f(d)

  def ask[A]: Transformation[A, Nothing, Nothing, A] = Transformation(Validation.ask[A])(identity)

  def lift[A, B](f: A => B)(g: B => A): Transformation[A, Nothing, Nothing, B] =
    Transformation(Validation.lift(f))(g)
