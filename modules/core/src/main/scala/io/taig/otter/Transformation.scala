package io.taig.otter

import io.taig.otter.validation.Validation
import cats.syntax.all.*

trait Transformation[A, +B, +C, D]:
  final def imap[E](f: D => E)(g: E => D): Transformation[A, B, C, E] = ???

  final def ivalidate[E, F, G](validation: Validation[D, E, F, G])(f: G => D): Transformation[A, B | E, C | F, G] = ???

  def validate_[E, F](validation: Validation[D, E, F, Unit]): Transformation[A, B | E, C | F, D] =
    ivalidate(validation.tap)(identity)

  final def mapValidation[E, F](
      f: Validation[A, B, C, D] => Validation[A, E, F, D]
  ): Transformation[A, E, F, D] = ???

object Transformation:
  type Plain[A, D] = Transformation[A, Nothing, Nothing, D]

  def apply[A, B, C, D](validation: Validation[A, B, C, D])(f: D => A): Transformation[A, B, C, D] =
    ???

  def ask[A]: Transformation[A, Nothing, Nothing, A] = Transformation(Validation.ask[A])(identity)

  def lift[A, B](f: A => B)(g: B => A): Transformation[A, Nothing, Nothing, B] =
    Transformation(Validation.lift(f))(g)
