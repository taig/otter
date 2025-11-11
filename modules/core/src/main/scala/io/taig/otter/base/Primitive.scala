package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Constraint
import io.taig.validation.Constraint.Primitive.Text
import io.taig.validation.Validation

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Primitive[A] extends Primitive.Read[A], Primitive.Write[A]:
  def imap[T](f: A => T)(g: T => A): Primitive[T]

object Primitive:
  sealed trait Read[+A] extends Product, Serializable:
    def constraints: Chain[Constraint.Primitive]

    def map[T](f: A => T): Primitive.Read[T]

  object Read:
    given Functor[Primitive.Read] with
      final override def map[A, B](fa: Primitive.Read[A])(f: A => B): Primitive.Read[B] = fa.map(f)

  sealed trait Write[-A] extends Product, Serializable:
    def contramap[T](f: T => A): Primitive.Write[T]

  object Write:
    given Contravariant[Primitive.Write] with
      final override def contramap[A, B](fa: Primitive.Write[A])(f: B => A): Primitive.Write[B] = fa.contramap(f)

  sealed abstract class Boolean[A] extends Primitive[A], Primitive.Boolean.Read[A], Primitive.Boolean.Write[A]:
    final override def imap[T](f: A => T)(g: T => A): Primitive.Boolean[T] = Boolean.Modify(self = this, f, g)

  object Boolean:
    sealed trait Read[+A] extends Primitive.Read[A]:
      override def constraints: Chain[Constraint.Primitive] = Chain.empty

      final override def map[T](f: A => T): Primitive.Boolean.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.Boolean.Read[A], f: A => B) extends Primitive.Boolean.Read[B]

      case object Root extends Primitive.Boolean.Read[SBoolean]

      given Functor[Primitive.Boolean.Read] with
        final override def map[A, B](fa: Primitive.Boolean.Read[A])(f: A => B): Primitive.Boolean.Read[B] = fa.map(f)

      given Self.Primitive.Boolean.Read[Primitive.Boolean.Read] with
        override def boolean: Primitive.Boolean.Read[SBoolean] = Root

    sealed trait Write[-A] extends Primitive.Write[A]:
      final override def contramap[T](f: T => A): Primitive.Boolean.Write[T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.Boolean.Write[A], f: B => A) extends Primitive.Boolean.Write[B]

      case object Root extends Primitive.Boolean.Write[SBoolean]

      given Contravariant[Primitive.Boolean.Write] with
        final override def contramap[A, B](fa: Primitive.Boolean.Write[A])(f: B => A): Primitive.Boolean.Write[B] =
          fa.contramap(f)

      given Self.Primitive.Boolean.Write[Primitive.Boolean.Write] with
        override def boolean: Primitive.Boolean.Write[SBoolean] = Root

    final case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A) extends Primitive.Boolean[B]

    case object Root extends Primitive.Boolean[SBoolean]

    given Invariant[Primitive.Boolean] with
      final override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] =
        fa.imap(f)(g)

    given Self.Primitive.Boolean[Primitive.Boolean] with
      override def boolean: Boolean[SBoolean] = Root

  sealed abstract class Number[A] extends Primitive[A], Primitive.Number.Read[A], Primitive.Number.Write[A]:
    final override def imap[T](f: A => T)(g: T => A): Primitive.Number[T] = Number.Modify(self = this, f, g)

  object Number:
    sealed trait Read[+A] extends Primitive.Read[A]:
      def constraints: Chain[Constraint.Primitive.Number]

      final override def map[T](f: A => T): Primitive.Number.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class BigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal])
          extends Primitive.Number.Read[JBigDecimal]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
          extends Primitive.Number.Read[JBigInteger]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
          extends Primitive.Number.Read[SDouble]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat])
          extends Primitive.Number.Read[SFloat]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Int(validation: Validation[Constraint.Primitive.Number, SInt])
          extends Primitive.Number.Read[SInt]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Long(validation: Validation[Constraint.Primitive.Number, SLong])
          extends Primitive.Number.Read[SLong]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Modify[A, B](self: Primitive.Number.Read[A], f: A => B) extends Primitive.Number.Read[B]:
        export self.constraints

      given Functor[Primitive.Number.Read] with
        final override def map[A, B](fa: Primitive.Number.Read[A])(f: A => B): Primitive.Number.Read[B] =
          fa.map(f)

      given Self.Primitive.Number.Read[Primitive.Number.Read] with
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): Primitive.Number.Read[JBigDecimal] =
          BigDecimal(validation)

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): Primitive.Number.Read[JBigInteger] =
          BigInteger(validation)

        override def constraints[A](self: Primitive.Number.Read[A]): Chain[Constraint.Primitive.Number] =
          self.constraints

        override def double(
            validation: Validation[Constraint.Primitive.Number, SDouble]
        ): Primitive.Number.Read[SDouble] =
          Double(validation)

        override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): Primitive.Number.Read[SFloat] =
          Float(validation)

        override def int(validation: Validation[Constraint.Primitive.Number, SInt]): Primitive.Number.Read[SInt] =
          Int(validation)

        override def long(validation: Validation[Constraint.Primitive.Number, SLong]): Primitive.Number.Read[SLong] =
          Long(validation)

    sealed trait Write[-A] extends Primitive.Write[A]:
      final override def contramap[T](f: T => A): Primitive.Number.Write[T] = Write.Modify(self = this, f)

    object Write:
      case object BigDecimal extends Primitive.Number.Write[JBigDecimal]

      case object BigInteger extends Primitive.Number.Write[JBigInteger]

      case object Double extends Primitive.Number.Write[SDouble]

      case object Float extends Primitive.Number.Write[SFloat]

      case object Int extends Primitive.Number.Write[SInt]

      case object Long extends Primitive.Number.Write[SLong]

      final case class Modify[A, B](self: Primitive.Number.Write[A], f: B => A) extends Primitive.Number.Write[B]

      given Contravariant[Primitive.Number.Write] with
        final override def contramap[A, B](fa: Primitive.Number.Write[A])(f: B => A): Primitive.Number.Write[B] =
          fa.contramap(f)

      given Self.Primitive.Number.Write[Primitive.Number.Write] with
        override def bigDecimal: Primitive.Number.Write[JBigDecimal] = BigDecimal

        override def bigInteger: Primitive.Number.Write[JBigInteger] = BigInteger

        override def double: Primitive.Number.Write[SDouble] = Double

        override def float: Primitive.Number.Write[SFloat] = Float

        override def int: Primitive.Number.Write[SInt] = Int

        override def long: Primitive.Number.Write[SLong] = Long

    final case class BigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal])
        extends Primitive.Number[JBigDecimal]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
        extends Primitive.Number[JBigInteger]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
        extends Primitive.Number[SDouble]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat])
        extends Primitive.Number[SFloat]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Int(validation: Validation[Constraint.Primitive.Number, SInt]) extends Primitive.Number[SInt]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Long(validation: Validation[Constraint.Primitive.Number, SLong]) extends Primitive.Number[SLong]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Modify[A, B](self: Primitive.Number[A], f: A => B, g: B => A) extends Primitive.Number[B]:
      export self.constraints

    given Invariant[Primitive.Number] with
      final override def imap[A, B](fa: Primitive.Number[A])(f: A => B)(g: B => A): Primitive.Number[B] =
        fa.imap(f)(g)

    given Self.Primitive.Number[Primitive.Number] with
      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): Primitive.Number[JBigDecimal] =
        BigDecimal(validation)

      override def bigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): Primitive.Number[JBigInteger] =
        BigInteger(validation)

      override def constraints[A](self: Primitive.Number[A]): Chain[Constraint.Primitive.Number] = self.constraints

      override def double(validation: Validation[Constraint.Primitive.Number, SDouble]): Primitive.Number[SDouble] =
        Double(validation)

      override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): Primitive.Number[SFloat] =
        Float(validation)

      override def int(validation: Validation[Constraint.Primitive.Number, SInt]): Primitive.Number[SInt] =
        Int(validation)

      override def long(validation: Validation[Constraint.Primitive.Number, SLong]): Primitive.Number[SLong] =
        Long(validation)

  sealed abstract class Text[A] extends Primitive[A], Primitive.Text.Read[A], Primitive.Text.Write[A]:
    final override def imap[T](f: A => T)(g: T => A): Primitive.Text[T] = Text.Modify(self = this, f, g)

  object Text:
    sealed trait Read[+A] extends Primitive.Read[A]:
      def constraints: Chain[Constraint.Primitive.Text]

      final override def map[T](f: A => T): Primitive.Text.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.Text.Read[A], f: A => B) extends Primitive.Text.Read[B]:
        export self.constraints

      final case class Parser[A](name: JString, parse: JString => Either[JString, A]) extends Primitive.Text.Read[A]:
        override def constraints: Chain[Constraint.Primitive.Text] = Chain.empty

      final case class Root(validation: Validation[Constraint.Primitive.Text, JString])
          extends Primitive.Text.Read[JString]:
        override def constraints: Chain[Constraint.Primitive.Text] = validation.constraints

      given Functor[Primitive.Text.Read] with
        final override def map[A, B](fa: Primitive.Text.Read[A])(f: A => B): Primitive.Text.Read[B] =
          fa.map(f)

      given Self.Primitive.Text.Read[Primitive.Text.Read] with
        override def constraints[A](self: Primitive.Text.Read[A]): Chain[Constraint.Primitive.Text] = self.constraints

        override def parser[A](name: JString, parse: JString => Either[JString, A]): Primitive.Text.Read[A] =
          Parser(name, parse)

        override def string(validation: Validation[Constraint.Primitive.Text, JString]): Primitive.Text.Read[JString] =
          Root(validation)

    sealed trait Write[-A] extends Primitive.Write[A]:
      final override def contramap[T](f: T => A): Primitive.Text.Write[T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.Text.Write[A], f: B => A) extends Primitive.Text.Write[B]

      final case class Printer[A](name: JString, print: A => JString) extends Primitive.Text.Write[A]

      case object Root extends Primitive.Text.Write[JString]

      given Contravariant[Primitive.Text.Write] with
        final override def contramap[A, B](fa: Primitive.Text.Write[A])(f: B => A): Primitive.Text.Write[B] =
          fa.contramap(f)

      given Self.Primitive.Text.Write[Primitive.Text.Write] with
        override def printer[A](name: JString, print: A => JString): Primitive.Text.Write[A] = Printer(name, print)

        override def string: Primitive.Text.Write[JString] = Root

    final case class Codec[A](name: JString, parse: JString => Either[JString, A], print: A => JString)
        extends Primitive.Text[A]:
      override def constraints: Chain[Constraint.Primitive.Text] = Chain.empty

    final case class Modify[A, B](self: Primitive.Text[A], f: A => B, g: B => A) extends Primitive.Text[B]:
      export self.constraints

    final case class Root(validation: Validation[Constraint.Primitive.Text, JString]) extends Primitive.Text[JString]:
      override def constraints: Chain[Constraint.Primitive.Text] = validation.constraints

    given Invariant[Primitive.Text] with
      final override def imap[A, B](fa: Primitive.Text[A])(f: A => B)(g: B => A): Primitive.Text[B] = fa.imap(f)(g)

    given Self.Primitive.Text[Primitive.Text] with
      override def codec[A](
          name: JString,
          parse: JString => Either[JString, A],
          print: A => JString
      ): Primitive.Text[A] = Codec(name, parse, print)

      override def constraints[A](self: Primitive.Text[A]): Chain[Constraint.Primitive.Text] = self.constraints

      override def string(validation: Validation[Constraint.Primitive.Text, JString]): Primitive.Text[JString] =
        Root(validation)

  given Invariant[Primitive] with
    final override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)
