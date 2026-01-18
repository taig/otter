package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.otter.InvariantK
import io.taig.otter.Reference
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean

object PrimitiveOperation:
  trait Boolean[F[_]]:
    self =>

    def boolean: F[SBoolean]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Boolean[G] = new Boolean[G]:
      override def boolean: G[SBoolean] = fK(self.boolean)

  object Boolean:
    trait Read[F[_]] extends PrimitiveOperation.Boolean[F]:
      self =>

      final override def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Boolean.Read[G] =
        new Read[G]:
          override def boolean: G[SBoolean] = fK(self.boolean)

    object Read:
      inline def apply[F[_]](using self: PrimitiveOperation.Boolean.Read[F]): PrimitiveOperation.Boolean.Read[F] = self

      given InvariantK[PrimitiveOperation.Boolean.Read]:
        extension [G[_]](fa: PrimitiveOperation.Boolean.Read[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(
              gK: [A] => H[A] => G[A]
          ): PrimitiveOperation.Boolean.Read[H] = fa.mapK(fK)

    trait Write[F[_]] extends PrimitiveOperation.Boolean[F]:
      self =>

      final override def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Boolean.Write[G] =
        new Write[G]:
          override def boolean: G[SBoolean] = fK(self.boolean)

    object Write:
      inline def apply[F[_]](using self: PrimitiveOperation.Boolean.Write[F]): PrimitiveOperation.Boolean.Write[F] =
        self

      given InvariantK[PrimitiveOperation.Boolean.Write]:
        extension [G[_]](fa: PrimitiveOperation.Boolean.Write[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(
              gK: [A] => H[A] => G[A]
          ): PrimitiveOperation.Boolean.Write[H] = fa.mapK(fK)

    inline def apply[F[_]](using self: PrimitiveOperation.Boolean[F]): PrimitiveOperation.Boolean[F] = self

    given InvariantK[PrimitiveOperation.Boolean]:
      extension [G[_]](fa: PrimitiveOperation.Boolean[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(
            gK: [A] => H[A] => G[A]
        ): PrimitiveOperation.Boolean[H] = fa.mapK(fK)

  object Coerce:
    trait Boolean[F[_], G[_]]:
      self =>

      def lift[A](schema: Reference[G, A]): F[A]

      def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Boolean[H, G] = new Boolean[H, G]:
        override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

    object Boolean:
      trait Read[F[_], G[_]] extends PrimitiveOperation.Coerce.Boolean[F, G]:
        self =>

        final override def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Boolean.Read[H, G] =
          new Read[H, G]:
            override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

      object Read:
        inline def apply[F[_], G[_]](using
            self: PrimitiveOperation.Coerce.Boolean.Read[F, G]
        ): PrimitiveOperation.Coerce.Boolean.Read[F, G] = self

        given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Boolean.Read[f, F]]:
          extension [G[_]](fa: PrimitiveOperation.Coerce.Boolean.Read[G, F])
            override def imapK[H[_]](fK: [A] => G[A] => H[A])(
                gK: [A] => H[A] => G[A]
            ): PrimitiveOperation.Coerce.Boolean.Read[H, F] = fa.mapK(fK)

      trait Write[F[_], G[_]] extends PrimitiveOperation.Coerce.Boolean[F, G]:
        self =>

        final override def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Boolean.Write[H, G] =
          new Write[H, G]:
            override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

      object Write:
        inline def apply[F[_], G[_]](using
            self: PrimitiveOperation.Coerce.Boolean.Write[F, G]
        ): PrimitiveOperation.Coerce.Boolean.Write[F, G] = self

        given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Boolean.Write[f, F]]:
          extension [G[_]](fa: PrimitiveOperation.Coerce.Boolean.Write[G, F])
            override def imapK[H[_]](fK: [A] => G[A] => H[A])(
                gK: [A] => H[A] => G[A]
            ): PrimitiveOperation.Coerce.Boolean.Write[H, F] = fa.mapK(fK)

      inline def apply[F[_], G[_]](using
          self: PrimitiveOperation.Coerce.Boolean[F, G]
      ): PrimitiveOperation.Coerce.Boolean[F, G] = self

      given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Boolean[f, F]]:
        extension [G[_]](fa: PrimitiveOperation.Coerce.Boolean[G, F])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(
              gK: [A] => H[A] => G[A]
          ): PrimitiveOperation.Coerce.Boolean[H, F] = fa.mapK(fK)

    trait Number[F[_], G[_]]:
      self =>

      def lift[A](schema: Reference[G, A]): F[A]

      def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Number[H, G] = new Number[H, G]:
        override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

    object Number:
      trait Read[F[_], G[_]] extends PrimitiveOperation.Coerce.Number[F, G]:
        self =>

        final override def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Number.Read[H, G] =
          new Read[H, G]:
            override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

      object Read:
        inline def apply[F[_], G[_]](using
            self: PrimitiveOperation.Coerce.Number.Read[F, G]
        ): PrimitiveOperation.Coerce.Number.Read[F, G] = self

        given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Number.Read[f, F]]:
          extension [G[_]](fa: PrimitiveOperation.Coerce.Number.Read[G, F])
            override def imapK[H[_]](fK: [A] => G[A] => H[A])(
                gK: [A] => H[A] => G[A]
            ): PrimitiveOperation.Coerce.Number.Read[H, F] = fa.mapK(fK)

      trait Write[F[_], G[_]] extends PrimitiveOperation.Coerce.Number[F, G]:
        self =>

        final override def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Number.Write[H, G] =
          new Write[H, G]:
            override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

      object Write:
        inline def apply[F[_], G[_]](using
            self: PrimitiveOperation.Coerce.Number.Write[F, G]
        ): PrimitiveOperation.Coerce.Number.Write[F, G] = self

        given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Number.Write[f, F]]:
          extension [G[_]](fa: PrimitiveOperation.Coerce.Number.Write[G, F])
            override def imapK[H[_]](fK: [A] => G[A] => H[A])(
                gK: [A] => H[A] => G[A]
            ): PrimitiveOperation.Coerce.Number.Write[H, F] = fa.mapK(fK)

      inline def apply[F[_], G[_]](using
          self: PrimitiveOperation.Coerce.Number[F, G]
      ): PrimitiveOperation.Coerce.Number[F, G] = self

      given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Number[f, F]]:
        extension [G[_]](fa: PrimitiveOperation.Coerce.Number[G, F])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(
              gK: [A] => H[A] => G[A]
          ): PrimitiveOperation.Coerce.Number[H, F] = fa.mapK(fK)

    trait Text[F[_], G[_]]:
      self =>

      def lift[A](schema: Reference[G, A]): F[A]

      def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Text[H, G] = new Text[H, G]:
        override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

    object Text:
      trait Read[F[_], G[_]] extends PrimitiveOperation.Coerce.Text[F, G]:
        self =>

        final override def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Text.Read[H, G] =
          new Read[H, G]:
            override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

      object Read:
        inline def apply[F[_], G[_]](using
            self: PrimitiveOperation.Coerce.Text.Read[F, G]
        ): PrimitiveOperation.Coerce.Text.Read[F, G] = self

        given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Text.Read[f, F]]:
          extension [G[_]](fa: PrimitiveOperation.Coerce.Text.Read[G, F])
            override def imapK[H[_]](fK: [A] => G[A] => H[A])(
                gK: [A] => H[A] => G[A]
            ): PrimitiveOperation.Coerce.Text.Read[H, F] = fa.mapK(fK)

      trait Write[F[_], G[_]] extends PrimitiveOperation.Coerce.Text[F, G]:
        self =>

        final override def mapK[H[_]](fK: [A] => F[A] => H[A]): PrimitiveOperation.Coerce.Text.Write[H, G] =
          new Write[H, G]:
            override def lift[A](schema: Reference[G, A]): H[A] = fK(self.lift(schema))

      object Write:
        inline def apply[F[_], G[_]](using
            self: PrimitiveOperation.Coerce.Text.Write[F, G]
        ): PrimitiveOperation.Coerce.Text.Write[F, G] = self

        given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Text.Write[f, F]]:
          extension [G[_]](fa: PrimitiveOperation.Coerce.Text.Write[G, F])
            override def imapK[H[_]](fK: [A] => G[A] => H[A])(
                gK: [A] => H[A] => G[A]
            ): PrimitiveOperation.Coerce.Text.Write[H, F] = fa.mapK(fK)

      inline def apply[F[_], G[_]](using
          self: PrimitiveOperation.Coerce.Text[F, G]
      ): PrimitiveOperation.Coerce.Text[F, G] = self

      given [F[_]] => InvariantK[[f[_]] =>> PrimitiveOperation.Coerce.Text[f, F]]:
        extension [G[_]](fa: PrimitiveOperation.Coerce.Text[G, F])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(
              gK: [A] => H[A] => G[A]
          ): PrimitiveOperation.Coerce.Text[H, F] = fa.mapK(fK)

  trait Number[F[_]]:
    self =>

    def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal]

    def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger]

    def double(validation: Validation[Constraint.Primitive.Number, Double]): F[Double]

    def float(validation: Validation[Constraint.Primitive.Number, Float]): F[Float]

    def int(validation: Validation[Constraint.Primitive.Number, Int]): F[Int]

    def long(validation: Validation[Constraint.Primitive.Number, Long]): F[Long]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Number[G] = new Number[G]:
      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): G[JBigDecimal] = fK(self.bigDecimal(validation))

      override def bigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): G[JBigInteger] = fK(self.bigInteger(validation))

      override def double(
          validation: Validation[Constraint.Primitive.Number, Double]
      ): G[Double] = fK(self.double(validation))

      override def float(
          validation: Validation[Constraint.Primitive.Number, Float]
      ): G[Float] = fK(self.float(validation))

      override def int(
          validation: Validation[Constraint.Primitive.Number, Int]
      ): G[Int] = fK(self.int(validation))

      override def long(
          validation: Validation[Constraint.Primitive.Number, Long]
      ): G[Long] = fK(self.long(validation))

  object Number:
    trait Read[F[_]] extends PrimitiveOperation.Number[F]:
      self =>

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Number.Read[G] = new Read[G]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): G[JBigDecimal] = fK(self.bigDecimal(validation))

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): G[JBigInteger] = fK(self.bigInteger(validation))

        override def double(
            validation: Validation[Constraint.Primitive.Number, Double]
        ): G[Double] = fK(self.double(validation))

        override def float(
            validation: Validation[Constraint.Primitive.Number, Float]
        ): G[Float] = fK(self.float(validation))

        override def int(
            validation: Validation[Constraint.Primitive.Number, Int]
        ): G[Int] = fK(self.int(validation))

        override def long(
            validation: Validation[Constraint.Primitive.Number, Long]
        ): G[Long] = fK(self.long(validation))

    object Read:
      inline def apply[F[_]](using self: PrimitiveOperation.Number.Read[F]): PrimitiveOperation.Number.Read[F] = self

      given InvariantK[PrimitiveOperation.Number.Read]:
        extension [F[_]](fa: PrimitiveOperation.Number.Read[F])
          override def imapK[G[_]](fK: [A] => F[A] => G[A])(
              gK: [A] => G[A] => F[A]
          ): PrimitiveOperation.Number.Read[G] = fa.mapK(fK)

    trait Write[F[_]] extends PrimitiveOperation.Number[F]:
      self =>

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Number.Write[G] = new Write[G]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): G[JBigDecimal] = fK(self.bigDecimal(validation))

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): G[JBigInteger] = fK(self.bigInteger(validation))

        override def double(
            validation: Validation[Constraint.Primitive.Number, Double]
        ): G[Double] = fK(self.double(validation))

        override def float(
            validation: Validation[Constraint.Primitive.Number, Float]
        ): G[Float] = fK(self.float(validation))

        override def int(
            validation: Validation[Constraint.Primitive.Number, Int]
        ): G[Int] = fK(self.int(validation))

        override def long(
            validation: Validation[Constraint.Primitive.Number, Long]
        ): G[Long] = fK(self.long(validation))

    object Write:
      inline def apply[F[_]](using self: PrimitiveOperation.Number.Write[F]): PrimitiveOperation.Number.Write[F] =
        self

      given InvariantK[PrimitiveOperation.Number.Write]:
        extension [F[_]](fa: PrimitiveOperation.Number.Write[F])
          override def imapK[G[_]](fK: [A] => F[A] => G[A])(
              gK: [A] => G[A] => F[A]
          ): PrimitiveOperation.Number.Write[G] = fa.mapK(fK)

    inline def apply[F[_]](using self: PrimitiveOperation.Number[F]): PrimitiveOperation.Number[F] = self

    given InvariantK[PrimitiveOperation.Number]:
      extension [F[_]](fa: PrimitiveOperation.Number[F])
        override def imapK[G[_]](fK: [A] => F[A] => G[A])(
            gK: [A] => G[A] => F[A]
        ): PrimitiveOperation.Number[G] = fa.mapK(fK)

  trait Text[F[_]]:
    self =>

    def codec[A](name: String, parse: String => Either[String, A], print: A => String): F[A]

    def string(validation: Validation[Constraint.Primitive.Text, String]): F[String]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Text[G] = new Text[G]:
      override def codec[A](name: String, parse: String => Either[String, A], print: A => String): G[A] =
        fK(self.codec(name, parse, print))

      override def string(
          validation: Validation[Constraint.Primitive.Text, String]
      ): G[String] = fK(self.string(validation))

  object Text:
    trait Read[F[_]] extends PrimitiveOperation.Text[F]:
      self =>

      def parser[A](name: String, parse: String => Either[String, A]): F[A]

      final override def codec[A](name: String, parse: String => Either[String, A], print: A => String): F[A] =
        parser(name, parse)

      final override def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Text.Read[G] = new Read[G]:
        override def parser[A](name: String, parse: String => Either[String, A]): G[A] =
          fK(self.parser(name, parse))

        override def string(
            validation: Validation[Constraint.Primitive.Text, String]
        ): G[String] = fK(self.string(validation))

    object Read:
      inline def apply[F[_]](using self: PrimitiveOperation.Text.Read[F]): PrimitiveOperation.Text.Read[F] = self

      given InvariantK[PrimitiveOperation.Text.Read]:
        extension [F[_]](fa: PrimitiveOperation.Text.Read[F])
          override def imapK[G[_]](fK: [A] => F[A] => G[A])(
              gK: [A] => G[A] => F[A]
          ): PrimitiveOperation.Text.Read[G] = fa.mapK(fK)

    trait Write[F[_]] extends PrimitiveOperation.Text[F]:
      self =>

      def printer[A](name: String, print: A => String): F[A]

      final override def codec[A](name: String, parse: String => Either[String, A], print: A => String): F[A] =
        printer(name, print)

      final override def mapK[G[_]](fK: [A] => F[A] => G[A]): PrimitiveOperation.Text.Write[G] = new Write[G]:
        override def printer[A](name: String, print: A => String): G[A] = fK(self.printer(name, print))

        override def string(
            validation: Validation[Constraint.Primitive.Text, String]
        ): G[String] = fK(self.string(validation))

    object Write:
      inline def apply[F[_]](using self: PrimitiveOperation.Text.Write[F]): PrimitiveOperation.Text.Write[F] = self

      given InvariantK[PrimitiveOperation.Text.Write]:
        extension [F[_]](fa: PrimitiveOperation.Text.Write[F])
          override def imapK[G[_]](fK: [A] => F[A] => G[A])(
              gK: [A] => G[A] => F[A]
          ): PrimitiveOperation.Text.Write[G] = fa.mapK(fK)

    inline def apply[F[_]](using self: PrimitiveOperation.Text[F]): PrimitiveOperation.Text[F] = self

    given InvariantK[PrimitiveOperation.Text]:
      extension [F[_]](fa: PrimitiveOperation.Text[F])
        override def imapK[G[_]](fK: [A] => F[A] => G[A])(
            gK: [A] => G[A] => F[A]
        ): PrimitiveOperation.Text[G] = fa.mapK(fK)
