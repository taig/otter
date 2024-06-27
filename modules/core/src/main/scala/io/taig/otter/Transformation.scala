package io.taig.otter

import io.taig.otter.validation.Validation
import cats.syntax.all.*

trait Transformation[A, +B, +C, D] extends Transformation.Reader[A, B, C, D], Transformation.Writer[A, D]:
  final def imap[E](f: D => E)(g: E => D): Transformation[A, B, C, E] =
    Transformation(validation.map(f))(apply.compose(g))

  final def ivalidate[E, F, G](validation: Validation[D, E, F, G])(
      f: G => D
  ): Transformation[A, B | E, C | F, G] = Transformation(this.validation.andThen(validation))(apply.compose(f))

  override def validate_[E, F](validation: Validation[D, E, F, Unit]): Transformation[A, B | E, C | F, D] =
    ivalidate(validation.tap)(identity)

  final override def mapValidation[E, F](
      f: Validation[A, B, C, D] => Validation[A, E, F, D]
  ): Transformation[A, E, F, D] = Transformation(f(validation))(apply)

object Transformation:
  trait Reader[A, +B, +C, D]:
    def validation: Validation[A, B, C, D]

    def validate[E, F, G](validation: Validation[D, E, F, G]): Transformation.Reader[A, B | E, C | F, G] =
      Reader(this.validation.andThen(validation))

    def validate_[E, F](validation: Validation[D, E, F, Unit]): Transformation.Reader[A, B | E, C | F, D] =
      validate(validation.tap)

    final def map[E](f: D => E): Transformation.Reader[A, B, C, E] = Reader(validation.map(f))

    def mapValidation[E, F](f: Validation[A, B, C, D] => Validation[A, E, F, D]): Transformation.Reader[A, E, F, D] =
      Reader(f(validation))

  object Reader:
    def apply[A, B, C, D](validation: Validation[A, B, C, D]): Transformation.Reader[A, B, C, D] =
      val _validation = validation
      new Reader:
        override def validation: Validation[A, B, C, D] = _validation

    def lift[A, B](f: A => B): Transformation.Reader[A, Nothing, Nothing, B] = Reader(Validation.lift(f))

  trait Writer[A, B]:
    self =>

    def apply(b: B): A

    final def contramap[C](f: C => B): Transformation.Writer[A, C] = c => self.apply(f(c))

  object Writer:
    def apply[A, B](f: B => A): Transformation.Writer[A, B] = new Writer:
      override def apply(b: B): A = f(b)

  def apply[A, B, C, D](validation: Validation[A, B, C, D])(f: D => A): Transformation[A, B, C, D] =
    val _validation = validation
    new Transformation:
      override def validation: Validation[A, B, C, D] = _validation
      override def apply(d: D): A = f(d)

  def ask[A]: Transformation[A, Nothing, Nothing, A] = Transformation(Validation.ask[A])(identity)

  def lift[A, B](f: A => B)(g: B => A): Transformation[A, Nothing, Nothing, B] =
    Transformation(Validation.lift(f))(g)
