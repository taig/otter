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

type Primitive[A] = Primitive.Read[A] & Primitive.Write[A]

object Primitive:
  sealed trait Read[+A]:
    def map[B](f: A => B): Primitive.Read[B]

  object Read:
    given Functor[Primitive.Read]:
      override def map[A, B](fa: Primitive.Read[A])(f: A => B): Primitive.Read[B] = fa.map(f)

  sealed trait Write[-A]:
    def contramap[B](f: B => A): Primitive.Write[B]

  object Write:
    given Contravariant[Primitive.Write]:
      override def contramap[A, B](fa: Primitive.Write[A])(f: B => A): Primitive.Write[B] = fa.contramap(f)

  type Boolean[A] = Primitive.Boolean.Read[A] & Primitive.Boolean.Write[A]

  object Boolean:
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def map[B](f: A => B): Primitive.Boolean.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.Boolean.Read[A], f: A => B) extends Primitive.Boolean.Read[B]

      given Functor[Primitive.Boolean.Read]:
        override def map[A, B](fa: Primitive.Boolean.Read[A])(f: A => B): Primitive.Boolean.Read[B] =
          fa.map(f)

      given PrimitiveOperation.Boolean.Read[Primitive.Boolean.Read]:
        override def boolean: Primitive.Boolean.Read[SBoolean] = Root

    sealed trait Write[-A] extends Primitive.Write[A]:
      override def contramap[B](f: B => A): Primitive.Boolean.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.Boolean.Write[A], f: B => A) extends Primitive.Boolean.Write[B]

      given Contravariant[Primitive.Boolean.Write]:
        override def contramap[A, B](fa: Primitive.Boolean.Write[A])(f: B => A): Primitive.Boolean.Write[B] =
          fa.contramap(f)

      given PrimitiveOperation.Boolean.Write[Primitive.Boolean.Write]:
        override def boolean: Primitive.Boolean.Write[SBoolean] = Root

    final case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A)
        extends Primitive.Boolean.Read[B],
          Primitive.Boolean.Write[B]

    case object Root extends Primitive.Boolean.Read[SBoolean], Primitive.Boolean.Write[SBoolean]

    given Invariant[Primitive.Boolean]:
      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] =
        Modify(fa, f, g)

    given PrimitiveOperation.Boolean[Primitive.Boolean]:
      override def boolean: Primitive.Boolean[SBoolean] = Root

  type Number[A] = Primitive.Number.Read[A] & Primitive.Number.Write[A]

  object Number:
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def map[B](f: A => B): Primitive.Number.Read[B] = Read.Modify(self = this, f)

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

    sealed trait Write[-A] extends Primitive.Write[A]:
      override def contramap[B](f: B => A): Primitive.Number.Write[B] = Write.Modify(self = this, f)

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
          Primitive.Number.Write[JBigDecimal]

    final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
        extends Primitive.Number.Read[JBigInteger],
          Primitive.Number.Write[JBigInteger]

    final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
        extends Primitive.Number.Read[SDouble],
          Primitive.Number.Write[SDouble]

    final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat])
        extends Primitive.Number.Read[SFloat],
          Primitive.Number.Write[SFloat]

    final case class Int(validation: Validation[Constraint.Primitive.Number, SInt])
        extends Primitive.Number.Read[SInt],
          Primitive.Number.Write[SInt]

    final case class Long(validation: Validation[Constraint.Primitive.Number, SLong])
        extends Primitive.Number.Read[SLong],
          Primitive.Number.Write[SLong]

    final case class Modify[A, B](self: Primitive.Number[A], f: A => B, g: B => A)
        extends Primitive.Number.Read[B],
          Primitive.Number.Write[B]

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
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def map[B](f: A => B): Primitive.Text.Read[B] = Read.Modify(self = this, f)

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

    sealed trait Write[-A] extends Primitive.Write[A]:
      override def contramap[B](f: B => A): Primitive.Text.Write[B] = Write.Modify(self = this, f)

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
          Primitive.Text.Write[A]

    final case class Modify[A, B](self: Primitive.Text[A], f: A => B, g: B => A)
        extends Primitive.Text.Read[B],
          Primitive.Text.Write[B]

    final case class Root(validation: Validation[Constraint.Primitive.Text, String])
        extends Primitive.Text.Read[String],
          Primitive.Text.Write[String]

    given Invariant[Primitive.Text]:
      override def imap[A, B](fa: Primitive.Text[A])(f: A => B)(g: B => A): Primitive.Text[B] = Modify(fa, f, g)

    given PrimitiveOperation.Text[Primitive.Text]:
      override def codec[A](name: String, parse: String => Either[String, A], print: A => String): Primitive.Text[A] =
        Codec(name, parse, print)

      override def string(
          validation: Validation[Constraint.Primitive.Text, String]
      ): Primitive.Text[String] = Root(validation)

  final case class Modify[A, B](self: Primitive[A], f: A => B, g: B => A) extends Primitive.Read[B], Primitive.Write[B]:
    override def map[C](h: B => C): Primitive.Read[C] = self.map(f andThen h)
    override def contramap[C](h: C => B): Primitive.Write[C] = self.contramap(h andThen g)

  given Invariant[Primitive]:
    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = Modify(fa, f, g)
