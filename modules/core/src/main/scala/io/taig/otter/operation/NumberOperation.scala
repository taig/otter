package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.Constraint.Primitive

trait NumberOperation[F[_]]:
  self =>

  def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal]

  def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger]

  def constraints[A](self: F[A]): Chain[Constraint.Primitive.Number]

  def double(validation: Validation[Constraint.Primitive.Number, Double]): F[Double]

  def float(validation: Validation[Constraint.Primitive.Number, Float]): F[Float]

  def int(validation: Validation[Constraint.Primitive.Number, Int]): F[Int]

  def long(validation: Validation[Constraint.Primitive.Number, Long]): F[Long]

  def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): NumberOperation[G] =
    new NumberOperation[G]:
      override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): G[JBigDecimal] =
        fK(self.bigDecimal(validation))

      override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): G[JBigInteger] =
        fK(self.bigInteger(validation))

      override def constraints[A](ga: G[A]): Chain[Primitive.Number] = self.constraints(gK(ga))

      override def double(validation: Validation[Constraint.Primitive.Number, Double]): G[Double] =
        fK(self.double(validation))

      override def float(validation: Validation[Constraint.Primitive.Number, Float]): G[Float] =
        fK(self.float(validation))

      override def int(validation: Validation[Constraint.Primitive.Number, Int]): G[Int] =
        fK(self.int(validation))

      override def long(validation: Validation[Constraint.Primitive.Number, Long]): G[Long] =
        fK(self.long(validation))

object NumberOperation:
  trait Read[F[_]] extends NumberOperation[F]:
    self =>

    override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): NumberOperation.Read[G] =
      new NumberOperation.Read[G]:
        override def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): G[JBigDecimal] =
          fK(self.bigDecimal(validation))

        override def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): G[JBigInteger] =
          fK(self.bigInteger(validation))

        override def constraints[A](ga: G[A]): Chain[Primitive.Number] = self.constraints(gK(ga))

        override def double(validation: Validation[Constraint.Primitive.Number, Double]): G[Double] =
          fK(self.double(validation))

        override def float(validation: Validation[Constraint.Primitive.Number, Float]): G[Float] =
          fK(self.float(validation))

        override def int(validation: Validation[Constraint.Primitive.Number, Int]): G[Int] =
          fK(self.int(validation))

        override def long(validation: Validation[Constraint.Primitive.Number, Long]): G[Long] =
          fK(self.long(validation))

  object Read:
    inline def apply[F[_]](using self: NumberOperation.Read[F]): NumberOperation.Read[F] = self

    given InvariantK[NumberOperation.Read] with
      extension [F[_]](self: NumberOperation.Read[F])
        override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): NumberOperation.Read[G] =
          self.imapK(fK)(gK)

  trait Write[F[_]] extends NumberOperation[F]:
    self =>

    def bigDecimal: F[JBigDecimal]

    final override def bigDecimal(validation: Validation[Primitive.Number, JBigDecimal]): F[JBigDecimal] =
      bigDecimal

    def bigInteger: F[JBigInteger]

    final override def bigInteger(validation: Validation[Primitive.Number, JBigInteger]): F[JBigInteger] =
      bigInteger

    final override def constraints[A](self: F[A]): Chain[Primitive.Number] = Chain.empty

    def double: F[Double]

    final override def double(validation: Validation[Primitive.Number, Double]): F[Double] = double

    def float: F[Float]

    final override def float(validation: Validation[Primitive.Number, Float]): F[Float] = float

    def int: F[Int]

    final override def int(validation: Validation[Primitive.Number, Int]): F[Int] = int

    def long: F[Long]

    final override def long(validation: Validation[Primitive.Number, Long]): F[Long] = long

    override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): NumberOperation.Write[G] =
      new NumberOperation.Write[G]:
        override def bigDecimal: G[JBigDecimal] = fK(self.bigDecimal)

        override def bigInteger: G[JBigInteger] = fK(self.bigInteger)

        override def double: G[Double] = fK(self.double)

        override def float: G[Float] = fK(self.float)

        override def int: G[Int] = fK(self.int)

        override def long: G[Long] = fK(self.long)

  object Write:
    inline def apply[F[_]](using self: NumberOperation.Write[F]): NumberOperation.Write[F] = self

    given InvariantK[NumberOperation.Write] with
      extension [F[_]](self: NumberOperation.Write[F])
        override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): NumberOperation.Write[G] =
          self.imapK(fK)(gK)

  inline def apply[Self[_]](using self: NumberOperation[Self]): NumberOperation[Self] = self

  given InvariantK[NumberOperation] with
    extension [F[_]](self: NumberOperation[F])
      override def imapK[G[_]](fK: [A] => F[A] => G[A])(gK: [A] => G[A] => F[A]): NumberOperation[G] =
        self.imapK(fK)(gK)
