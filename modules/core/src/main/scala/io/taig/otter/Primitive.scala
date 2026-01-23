package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

type Primitive[+F[_], A] = Primitive.Read[F, A] & Primitive.Write[F, A]

object Primitive:
  sealed trait Read[+F[_], +A]:
    def map[B](f: A => B): Primitive.Read[F, B]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Read[G, A]

  object Read:
    given [F[_]] => Functor[Primitive.Read[F, *]]:
      override def map[A, B](fa: Primitive.Read[F, A])(f: A => B): Primitive.Read[F, B] = fa.map(f)

  sealed trait Write[+F[_], -A]:
    def contramap[B](f: B => A): Primitive.Write[F, B]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Write[G, A]

  object Write:
    given [F[_]] => Contravariant[Primitive.Write[F, *]]:
      override def contramap[A, B](fa: Primitive.Write[F, A])(f: B => A): Primitive.Write[F, B] = fa.contramap(f)

  type Boolean[A] = Primitive.Boolean.Read[A] & Primitive.Boolean.Write[A]

  object Boolean:
    sealed trait Read[+A] extends Primitive.Read[Nothing, A]:
      override def map[B](f: A => B): Primitive.Boolean.Read[B] = Read.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Boolean.Read[A] = this

    object Read:
      final case class Modify[A, B](self: Primitive.Boolean.Read[A], f: A => B) extends Primitive.Boolean.Read[B]

      given Functor[Primitive.Boolean.Read]:
        override def map[A, B](fa: Primitive.Boolean.Read[A])(f: A => B): Primitive.Boolean.Read[B] =
          fa.map(f)

      given PrimitiveOperation.Boolean.Read[Primitive.Boolean.Read]:
        override def boolean: Primitive.Boolean.Read[SBoolean] = Root

    sealed trait Write[-A] extends Primitive.Write[Nothing, A]:
      override def contramap[B](f: B => A): Primitive.Boolean.Write[B] = Write.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Boolean.Write[A] = this

    object Write:
      final case class Modify[A, B](self: Primitive.Boolean.Write[A], f: B => A) extends Primitive.Boolean.Write[B]

      given Contravariant[Primitive.Boolean.Write]:
        override def contramap[A, B](fa: Primitive.Boolean.Write[A])(f: B => A): Primitive.Boolean.Write[B] =
          fa.contramap(f)

      given PrimitiveOperation.Boolean.Write[Primitive.Boolean.Write]:
        override def boolean: Primitive.Boolean.Write[SBoolean] = Root

    final case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A)
        extends Primitive.Boolean.Read[B],
          Primitive.Boolean.Write[B]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Boolean[B] = this

    case object Root extends Primitive.Boolean.Read[SBoolean], Primitive.Boolean.Write[SBoolean]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Boolean[SBoolean] = this

    given Invariant[Primitive.Boolean]:
      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] =
        Modify(fa, f, g)

    given PrimitiveOperation.Boolean[Primitive.Boolean]:
      override def boolean: Primitive.Boolean[SBoolean] = Root

  type Coerce[+F[_], A] = Primitive.Coerce.Read[F, A] & Primitive.Coerce.Write[F, A]

  object Coerce:
    sealed trait Read[+F[_], +A] extends Primitive.Read[F, A]:
      def schema: Reference[F, ?]
      override def map[B](f: A => B): Primitive.Coerce.Read[F, B] = Read.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Read[G, A]

    object Read:
      final case class Modify[F[_], A, B](self: Primitive.Coerce.Read[F, A], f: A => B)
          extends Primitive.Coerce.Read[F, B]:
        export self.schema

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Read[G, B] = copy(self = self.mapK(fK))

      given [F[_]] => Functor[Primitive.Coerce.Read[F, *]]:
        override def map[A, B](fa: Primitive.Coerce.Read[F, A])(f: A => B): Primitive.Coerce.Read[F, B] = fa.map(f)

    sealed trait Write[+F[_], -A] extends Primitive.Write[F, A]:
      def schema: Reference[F, ?]
      override def contramap[B](f: B => A): Primitive.Coerce.Write[F, B] = Write.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Write[G, A]

    object Write:
      final case class Modify[F[_], A, B](self: Primitive.Coerce.Write[F, A], f: B => A)
          extends Primitive.Coerce.Write[F, B]:
        export self.schema

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Write[G, B] = copy(self = self.mapK(fK))

      given [F[_]] => Contravariant[Primitive.Coerce.Write[F, *]]:
        override def contramap[A, B](fa: Primitive.Coerce.Write[F, A])(f: B => A): Primitive.Coerce.Write[F, B] =
          fa.contramap(f)

    final case class Modify[F[_], A, B](self: Primitive.Coerce[F, A], f: A => B, g: B => A)
        extends Primitive.Coerce.Read[F, B],
          Primitive.Coerce.Write[F, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce[G, B] = copy(self = self.mapK(fK))

    type Boolean[F[_], A] = Primitive.Coerce.Boolean.Read[F, A] & Primitive.Coerce.Boolean.Write[F, A]

    object Boolean:
      sealed trait Read[+F[_], +A] extends Primitive.Coerce.Read[F, A]:
        override def map[B](f: A => B): Primitive.Coerce.Boolean.Read[F, B] = Read.Modify(self = this, f)

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Boolean.Read[G, A]

      object Read:
        final case class Modify[F[_], A, B](self: Primitive.Coerce.Boolean.Read[F, A], f: A => B)
            extends Primitive.Coerce.Boolean.Read[F, B]:
          export self.schema

          override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Boolean.Read[G, B] =
            copy(self = self.mapK(fK))

        given [F[_]] => Functor[Primitive.Coerce.Boolean.Read[F, *]]:
          override def map[A, B](fa: Primitive.Coerce.Boolean.Read[F, A])(
              f: A => B
          ): Primitive.Coerce.Boolean.Read[F, B] = fa.map(f)

        given [F[_]] => PrimitiveOperation.Coerce.Boolean.Read[Primitive.Coerce.Boolean.Read[F, *], F]:
          override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Boolean.Read[F, A] = Root(schema)

      sealed trait Write[+F[_], -A] extends Primitive.Coerce.Write[F, A]:
        override def contramap[B](f: B => A): Primitive.Coerce.Boolean.Write[F, B] = Write.Modify(self = this, f)

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Boolean.Write[G, A]

      object Write:
        final case class Modify[F[_], A, B](self: Primitive.Coerce.Boolean.Write[F, A], f: B => A)
            extends Primitive.Coerce.Boolean.Write[F, B]:
          export self.schema

          override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Boolean.Write[G, B] =
            copy(self = self.mapK(fK))

        given [F[_]] => Contravariant[Primitive.Coerce.Boolean.Write[F, *]]:
          override def contramap[A, B](fa: Primitive.Coerce.Boolean.Write[F, A])(
              f: B => A
          ): Primitive.Coerce.Boolean.Write[F, B] = fa.contramap(f)

        given [F[_]] => PrimitiveOperation.Coerce.Boolean.Write[Primitive.Coerce.Boolean.Write[F, *], F]:
          override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Boolean.Write[F, A] = Root(schema)

      final case class Modify[F[_], A, B](self: Primitive.Coerce.Boolean[F, A], f: A => B, g: B => A)
          extends Primitive.Coerce.Boolean.Read[F, B],
            Primitive.Coerce.Boolean.Write[F, B]:
        export self.schema

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Boolean[G, B] = copy(self = self.mapK(fK))

      final case class Root[F[_], A](schema: Reference[F, A])
          extends Primitive.Coerce.Boolean.Read[F, A],
            Primitive.Coerce.Boolean.Write[F, A]:
        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Boolean[G, A] =
          copy(schema = schema.mapK[F, G](fK))

      given [F[_]] => Invariant[Primitive.Coerce.Boolean[F, *]]:
        override def imap[A, B](fa: Primitive.Coerce.Boolean[F, A])(f: A => B)(
            g: B => A
        ): Primitive.Coerce.Boolean[F, B] = Modify(fa, f, g)

      given [F[_]] => PrimitiveOperation.Coerce.Boolean[Primitive.Coerce.Boolean[F, *], F]:
        override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Boolean[F, A] = Root(schema)

    type Number[F[_], A] = Primitive.Coerce.Number.Read[F, A] & Primitive.Coerce.Number.Write[F, A]

    object Number:
      sealed trait Read[+F[_], +A] extends Primitive.Coerce.Read[F, A]:
        override def map[B](f: A => B): Primitive.Coerce.Number.Read[F, B] = Read.Modify(self = this, f)

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Number.Read[G, A]

      object Read:
        final case class Modify[F[_], A, B](self: Primitive.Coerce.Number.Read[F, A], f: A => B)
            extends Primitive.Coerce.Number.Read[F, B]:
          export self.schema

          override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Number.Read[G, B] =
            copy(self = self.mapK(fK))

        given [F[_]] => Functor[Primitive.Coerce.Number.Read[F, *]]:
          override def map[A, B](fa: Primitive.Coerce.Number.Read[F, A])(
              f: A => B
          ): Primitive.Coerce.Number.Read[F, B] = fa.map(f)

        given [F[_]] => PrimitiveOperation.Coerce.Number.Read[Primitive.Coerce.Number.Read[F, *], F]:
          override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Number.Read[F, A] = Root(schema)

      sealed trait Write[+F[_], -A] extends Primitive.Coerce.Write[F, A]:
        override def contramap[B](f: B => A): Primitive.Coerce.Number.Write[F, B] = Write.Modify(self = this, f)

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Number.Write[G, A]

      object Write:
        final case class Modify[F[_], A, B](self: Primitive.Coerce.Number.Write[F, A], f: B => A)
            extends Primitive.Coerce.Number.Write[F, B]:
          export self.schema

          override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Number.Write[G, B] =
            copy(self = self.mapK(fK))

        given [F[_]] => Contravariant[Primitive.Coerce.Number.Write[F, *]]:
          override def contramap[A, B](fa: Primitive.Coerce.Number.Write[F, A])(
              f: B => A
          ): Primitive.Coerce.Number.Write[F, B] = fa.contramap(f)

        given [F[_]] => PrimitiveOperation.Coerce.Number.Write[Primitive.Coerce.Number.Write[F, *], F]:
          override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Number.Write[F, A] = Root(schema)

      final case class Modify[F[_], A, B](self: Primitive.Coerce.Number[F, A], f: A => B, g: B => A)
          extends Primitive.Coerce.Number.Read[F, B],
            Primitive.Coerce.Number.Write[F, B]:
        export self.schema

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Number[G, B] = copy(self = self.mapK(fK))

      final case class Root[F[_], A](schema: Reference[F, A])
          extends Primitive.Coerce.Number.Read[F, A],
            Primitive.Coerce.Number.Write[F, A]:
        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Number[G, A] =
          copy(schema = schema.mapK[F, G](fK))

      given [F[_]] => Invariant[Primitive.Coerce.Number[F, *]]:
        override def imap[A, B](fa: Primitive.Coerce.Number[F, A])(f: A => B)(
            g: B => A
        ): Primitive.Coerce.Number[F, B] = Modify(fa, f, g)

      given [F[_]] => PrimitiveOperation.Coerce.Number[Primitive.Coerce.Number[F, *], F]:
        override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Number[F, A] = Root(schema)

    type Text[F[_], A] = Primitive.Coerce.Text.Read[F, A] & Primitive.Coerce.Text.Write[F, A]

    object Text:
      sealed trait Read[+F[_], +A] extends Primitive.Coerce.Read[F, A]:
        override def map[B](f: A => B): Primitive.Coerce.Text.Read[F, B] = Read.Modify(self = this, f)

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Text.Read[G, A]

      object Read:
        final case class Modify[F[_], A, B](self: Primitive.Coerce.Text.Read[F, A], f: A => B)
            extends Primitive.Coerce.Text.Read[F, B]:
          export self.schema

          override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Text.Read[G, B] =
            copy(self = self.mapK(fK))

        given [F[_]] => Functor[Primitive.Coerce.Text.Read[F, *]]:
          override def map[A, B](fa: Primitive.Coerce.Text.Read[F, A])(f: A => B): Primitive.Coerce.Text.Read[F, B] =
            fa.map(f)

        given [F[_]] => PrimitiveOperation.Coerce.Text.Read[Primitive.Coerce.Text.Read[F, *], F]:
          override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Text.Read[F, A] = Root(schema)

      sealed trait Write[+F[_], -A] extends Primitive.Coerce.Write[F, A]:
        override def contramap[B](f: B => A): Primitive.Coerce.Text.Write[F, B] = Write.Modify(self = this, f)

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Text.Write[G, A]

      object Write:
        final case class Modify[F[_], A, B](self: Primitive.Coerce.Text.Write[F, A], f: B => A)
            extends Primitive.Coerce.Text.Write[F, B]:
          export self.schema

          override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Text.Write[G, B] =
            copy(self = self.mapK(fK))

        given [F[_]] => Contravariant[Primitive.Coerce.Text.Write[F, *]]:
          override def contramap[A, B](fa: Primitive.Coerce.Text.Write[F, A])(
              f: B => A
          ): Primitive.Coerce.Text.Write[F, B] = fa.contramap(f)

        given [F[_]] => PrimitiveOperation.Coerce.Text.Write[Primitive.Coerce.Text.Write[F, *], F]:
          override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Text.Write[F, A] = Root(schema)

      final case class Modify[F[_], A, B](self: Primitive.Coerce.Text[F, A], f: A => B, g: B => A)
          extends Primitive.Coerce.Text.Read[F, B],
            Primitive.Coerce.Text.Write[F, B]:
        export self.schema

        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Text[G, B] = copy(self = self.mapK(fK))

      final case class Root[F[_], A](schema: Reference[F, A])
          extends Primitive.Coerce.Text.Read[F, A],
            Primitive.Coerce.Text.Write[F, A]:
        override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive.Coerce.Text[G, A] =
          copy(schema = schema.mapK[F, G](fK))

      given [F[_]] => Invariant[Primitive.Coerce.Text[F, *]]:
        override def imap[A, B](fa: Primitive.Coerce.Text[F, A])(f: A => B)(g: B => A): Primitive.Coerce.Text[F, B] =
          Modify(fa, f, g)

      given [F[_]] => PrimitiveOperation.Coerce.Text[Primitive.Coerce.Text[F, *], F]:
        override def lift[A](schema: Reference[F, A]): Primitive.Coerce.Text[F, A] = Root(schema)

    given [F[_]] => Invariant[Primitive.Coerce[F, *]]:
      override def imap[A, B](fa: Primitive.Coerce[F, A])(f: A => B)(g: B => A): Primitive.Coerce[F, B] =
        Modify(fa, f, g)

  type Number[A] = Primitive.Number.Read[A] & Primitive.Number.Write[A]

  object Number:
    sealed trait Read[+A] extends Primitive.Read[Nothing, A]:
      override def map[B](f: A => B): Primitive.Number.Read[B] = Read.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number.Read[A] = this

    object Read:
      final case class Modify[A, B](self: Primitive.Number.Read[A], f: A => B) extends Primitive.Number.Read[B]

      given Functor[Primitive.Number.Read]:
        override def map[A, B](fa: Primitive.Number.Read[A])(f: A => B): Primitive.Number.Read[B] = fa.map(f)

      given PrimitiveOperation.Number.Read[Primitive.Number.Read]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): Primitive.Number.Read[JBigDecimal] = BigDecimal(validation)
        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): Primitive.Number.Read[JBigInteger] = BigInteger(validation)
        override def double(
            validation: Validation[Constraint.Primitive.Number, SDouble]
        ): Primitive.Number.Read[SDouble] = Double(validation)
        override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): Primitive.Number.Read[SFloat] =
          Float(validation)
        override def int(validation: Validation[Constraint.Primitive.Number, SInt]): Primitive.Number.Read[SInt] =
          Int(validation)
        override def long(validation: Validation[Constraint.Primitive.Number, SLong]): Primitive.Number.Read[SLong] =
          Long(validation)

    sealed trait Write[-A] extends Primitive.Write[Nothing, A]:
      override def contramap[B](f: B => A): Primitive.Number.Write[B] = Write.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number.Write[A] = this

    object Write:
      final case class Modify[A, B](self: Primitive.Number.Write[A], f: B => A) extends Primitive.Number.Write[B]

      given Contravariant[Primitive.Number.Write]:
        override def contramap[A, B](fa: Primitive.Number.Write[A])(f: B => A): Primitive.Number.Write[B] =
          fa.contramap(f)

      given PrimitiveOperation.Number.Write[Primitive.Number.Write]:
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): Primitive.Number.Write[JBigDecimal] = BigDecimal(validation)
        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): Primitive.Number.Write[JBigInteger] = BigInteger(validation)
        override def double(
            validation: Validation[Constraint.Primitive.Number, SDouble]
        ): Primitive.Number.Write[SDouble] = Double(validation)
        override def float(
            validation: Validation[Constraint.Primitive.Number, SFloat]
        ): Primitive.Number.Write[SFloat] =
          Float(validation)
        override def int(validation: Validation[Constraint.Primitive.Number, SInt]): Primitive.Number.Write[SInt] =
          Int(validation)
        override def long(validation: Validation[Constraint.Primitive.Number, SLong]): Primitive.Number.Write[SLong] =
          Long(validation)

    final case class BigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal])
        extends Primitive.Number.Read[JBigDecimal],
          Primitive.Number.Write[JBigDecimal]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number[JBigDecimal] = this

    final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
        extends Primitive.Number.Read[JBigInteger],
          Primitive.Number.Write[JBigInteger]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number[JBigInteger] = this

    final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
        extends Primitive.Number.Read[SDouble],
          Primitive.Number.Write[SDouble]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number[SDouble] = this

    final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat])
        extends Primitive.Number.Read[SFloat],
          Primitive.Number.Write[SFloat]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number[SFloat] = this

    final case class Int(validation: Validation[Constraint.Primitive.Number, SInt])
        extends Primitive.Number.Read[SInt],
          Primitive.Number.Write[SInt]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number[SInt] = this

    final case class Long(validation: Validation[Constraint.Primitive.Number, SLong])
        extends Primitive.Number.Read[SLong],
          Primitive.Number.Write[SLong]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number[SLong] = this

    final case class Modify[A, B](self: Primitive.Number[A], f: A => B, g: B => A)
        extends Primitive.Number.Read[B],
          Primitive.Number.Write[B]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Number[B] = this

    given Invariant[Primitive.Number]:
      override def imap[A, B](fa: Primitive.Number[A])(f: A => B)(g: B => A): Primitive.Number[B] = Modify(fa, f, g)

    given PrimitiveOperation.Number[Primitive.Number]:
      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): Primitive.Number[JBigDecimal] = BigDecimal(validation)

      override def bigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): Primitive.Number[JBigInteger] = BigInteger(validation)

      override def double(
          validation: Validation[Constraint.Primitive.Number, SDouble]
      ): Primitive.Number[SDouble] = Double(validation)

      override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): Primitive.Number[SFloat] =
        Float(validation)

      override def int(validation: Validation[Constraint.Primitive.Number, SInt]): Primitive.Number[SInt] =
        Int(validation)

      override def long(validation: Validation[Constraint.Primitive.Number, SLong]): Primitive.Number[SLong] =
        Long(validation)

  type Text[A] = Primitive.Text.Read[A] & Primitive.Text.Write[A]

  object Text:
    sealed trait Read[+A] extends Primitive.Read[Nothing, A]:
      override def map[B](f: A => B): Primitive.Text.Read[B] = Read.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Text.Read[A] = this

    object Read:
      final case class Modify[A, B](self: Primitive.Text.Read[A], f: A => B) extends Primitive.Text.Read[B]

      final case class Parser[A](name: String, parse: String => Either[String, A]) extends Primitive.Text.Read[A]

      given Functor[Primitive.Text.Read]:
        override def map[A, B](fa: Primitive.Text.Read[A])(f: A => B): Primitive.Text.Read[B] = fa.map(f)

      given PrimitiveOperation.Text.Read[Primitive.Text.Read]:
        override def parser[A](name: String, parse: String => Either[String, A]): Primitive.Text.Read[A] =
          Parser(name, parse)

        override def string(
            validation: Validation[Constraint.Primitive.Text, String]
        ): Primitive.Text.Read[String] = Root(validation)

    sealed trait Write[-A] extends Primitive.Write[Nothing, A]:
      override def contramap[B](f: B => A): Primitive.Text.Write[B] = Write.Modify(self = this, f)

      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Text.Write[A] = this

    object Write:
      final case class Printer[A](name: String, print: A => String) extends Primitive.Text.Write[A]

      final case class Modify[A, B](self: Primitive.Text.Write[A], f: B => A) extends Primitive.Text.Write[B]

      given Contravariant[Primitive.Text.Write]:
        override def contramap[A, B](fa: Primitive.Text.Write[A])(f: B => A): Primitive.Text.Write[B] =
          fa.contramap(f)

      given PrimitiveOperation.Text.Write[Primitive.Text.Write]:
        override def printer[A](name: String, print: A => String): Primitive.Text.Write[A] =
          Printer(name, print)

        override def string(
            validation: Validation[Constraint.Primitive.Text, String]
        ): Primitive.Text.Write[String] = Root(validation)

    final case class Codec[A](name: String, parse: String => Either[String, A], print: A => String)
        extends Primitive.Text.Read[A],
          Primitive.Text.Write[A]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Text[A] = this

    final case class Modify[A, B](self: Primitive.Text[A], f: A => B, g: B => A)
        extends Primitive.Text.Read[B],
          Primitive.Text.Write[B]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Text[B] = this

    final case class Root(validation: Validation[Constraint.Primitive.Text, String])
        extends Primitive.Text.Read[String],
          Primitive.Text.Write[String]:
      override def mapK[G[_]](fK: [A] => Nothing => G[A]): Primitive.Text[String] = this

    given Invariant[Primitive.Text]:
      override def imap[A, B](fa: Primitive.Text[A])(f: A => B)(g: B => A): Primitive.Text[B] = Modify(fa, f, g)

    given PrimitiveOperation.Text[Primitive.Text]:
      override def codec[A](name: String, parse: String => Either[String, A], print: A => String): Primitive.Text[A] =
        Codec(name, parse, print)

      override def string(
          validation: Validation[Constraint.Primitive.Text, String]
      ): Primitive.Text[String] = Root(validation)

  final case class Modify[F[_], A, B](self: Primitive[F, A], f: A => B, g: B => A)
      extends Primitive.Read[F, B],
        Primitive.Write[F, B]:
    override def map[C](h: B => C): Primitive.Read[F, C] = self.map(f andThen h)
    override def contramap[C](h: C => B): Primitive.Write[F, C] = self.contramap(h andThen g)
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Primitive[G, B] = copy(self = self.mapK(fK))

  given [F[_]] => Invariant[Primitive[F, *]]:
    override def imap[A, B](fa: Primitive[F, A])(f: A => B)(g: B => A): Primitive[F, B] = Modify(fa, f, g)
