package io.taig.otter.operation

import io.taig.validation.Validation
import io.taig.otter.Constraint
import scala.Boolean as SBoolean
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object PrimitiveOperation:
  trait Boolean[Self[_]]:
    self =>

    def boolean: Self[SBoolean]

    def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Boolean[F] = new Boolean[F]:
      override def boolean: F[SBoolean] = fK(self.boolean)

  object Boolean:
    trait Read[Self[_]] extends PrimitiveOperation.Boolean[Self]:
      self =>

      final override def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Boolean.Read[F] =
        new Read[F]:
          override def boolean: F[SBoolean] = fK(self.boolean)

    object Read:
      inline def apply[Self[_]](using
          self: PrimitiveOperation.Boolean.Read[Self]
      ): PrimitiveOperation.Boolean.Read[Self] = self

    trait Write[Self[_]] extends PrimitiveOperation.Boolean[Self]:
      self =>

      final override def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Boolean.Write[F] =
        new Write[F]:
          override def boolean: F[SBoolean] = fK(self.boolean)

    object Write:
      inline def apply[Self[_]](using
          self: PrimitiveOperation.Boolean.Write[Self]
      ): PrimitiveOperation.Boolean.Write[Self] = self

    inline def apply[Self[_]](using
        self: PrimitiveOperation.Boolean[Self]
    ): PrimitiveOperation.Boolean[Self] = self

  trait Number[Self[_]]:
    self =>

    def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): Self[JBigDecimal]

    def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): Self[JBigInteger]

    def double(validation: Validation[Constraint.Primitive.Number, Double]): Self[Double]

    def float(validation: Validation[Constraint.Primitive.Number, Float]): Self[Float]

    def int(validation: Validation[Constraint.Primitive.Number, Int]): Self[Int]

    def long(validation: Validation[Constraint.Primitive.Number, Long]): Self[Long]

    def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Number[F] = new Number[F]:
      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): F[JBigDecimal] = fK(self.bigDecimal(validation))

      override def bigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): F[JBigInteger] = fK(self.bigInteger(validation))

      override def double(
          validation: Validation[Constraint.Primitive.Number, Double]
      ): F[Double] = fK(self.double(validation))

      override def float(
          validation: Validation[Constraint.Primitive.Number, Float]
      ): F[Float] = fK(self.float(validation))

      override def int(
          validation: Validation[Constraint.Primitive.Number, Int]
      ): F[Int] = fK(self.int(validation))

      override def long(
          validation: Validation[Constraint.Primitive.Number, Long]
      ): F[Long] = fK(self.long(validation))

  object Number:
    trait Read[Self[_]] extends PrimitiveOperation.Number[Self]:
      self =>

      override def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Number.Read[F] = new Read[F]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): F[JBigDecimal] = fK(self.bigDecimal(validation))

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): F[JBigInteger] = fK(self.bigInteger(validation))

        override def double(
            validation: Validation[Constraint.Primitive.Number, Double]
        ): F[Double] = fK(self.double(validation))

        override def float(
            validation: Validation[Constraint.Primitive.Number, Float]
        ): F[Float] = fK(self.float(validation))

        override def int(
            validation: Validation[Constraint.Primitive.Number, Int]
        ): F[Int] = fK(self.int(validation))

        override def long(
            validation: Validation[Constraint.Primitive.Number, Long]
        ): F[Long] = fK(self.long(validation))

      object Read:
        inline def apply[Self[_]](using
            self: PrimitiveOperation.Number.Read[Self]
        ): PrimitiveOperation.Number.Read[Self] = self

    trait Write[Self[_]] extends PrimitiveOperation.Number[Self]:
      self =>

      override def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Number.Write[F] = new Write[F]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): F[JBigDecimal] = fK(self.bigDecimal(validation))

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): F[JBigInteger] = fK(self.bigInteger(validation))

        override def double(
            validation: Validation[Constraint.Primitive.Number, Double]
        ): F[Double] = fK(self.double(validation))

        override def float(
            validation: Validation[Constraint.Primitive.Number, Float]
        ): F[Float] = fK(self.float(validation))

        override def int(
            validation: Validation[Constraint.Primitive.Number, Int]
        ): F[Int] = fK(self.int(validation))

        override def long(
            validation: Validation[Constraint.Primitive.Number, Long]
        ): F[Long] = fK(self.long(validation))

      object Read:
        inline def apply[Self[_]](using
            self: PrimitiveOperation.Number.Write[Self]
        ): PrimitiveOperation.Number.Write[Self] = self

    inline def apply[Self[_]](using
        self: PrimitiveOperation.Number[Self]
    ): PrimitiveOperation.Number[Self] = self

  trait Text[Self[_]]:
    self =>

    def codec[A](name: String, decode: String => Either[String, A], encode: A => String): Self[A]

    def string(validation: Validation[Constraint.Primitive.Text, String]): Self[String]

    def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Text[F] = new Text[F]:
      override def codec[A](name: String, decode: String => Either[String, A], encode: A => String): F[A] =
        fK(self.codec(name, decode, encode))

      override def string(
          validation: Validation[Constraint.Primitive.Text, String]
      ): F[String] = fK(self.string(validation))

  object Text:
    trait Read[Self[_]] extends PrimitiveOperation.Text[Self]:
      self =>

      final override def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Text.Read[F] = new Read[F]:
        override def codec[A](name: String, decode: String => Either[String, A], encode: A => String): F[A] =
          fK(self.codec(name, decode, encode))

        override def string(
            validation: Validation[Constraint.Primitive.Text, String]
        ): F[String] = fK(self.string(validation))

    object Read:
      inline def apply[Self[_]](using
          self: PrimitiveOperation.Text.Read[Self]
      ): PrimitiveOperation.Text.Read[Self] = self

    trait Write[Self[_]] extends PrimitiveOperation.Text[Self]:
      self =>

      final override def mapK[F[_]](fK: [A] => Self[A] => F[A]): PrimitiveOperation.Text.Write[F] = new Write[F]:
        override def codec[A](name: String, decode: String => Either[String, A], encode: A => String): F[A] =
          fK(self.codec(name, decode, encode))

        override def string(
            validation: Validation[Constraint.Primitive.Text, String]
        ): F[String] = fK(self.string(validation))

    object Write:
      inline def apply[Self[_]](using
          self: PrimitiveOperation.Text.Write[Self]
      ): PrimitiveOperation.Text.Write[Self] = self

    inline def apply[Self[_]](using
        self: PrimitiveOperation.Text[Self]
    ): PrimitiveOperation.Text[Self] = self
