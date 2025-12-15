package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Primitive
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

sealed abstract class PrimitiveBase[A] extends PrimitiveBase.Read[A], PrimitiveBase.Write[A]:
  def imap[T](f: A => T)(g: T => A): PrimitiveBase[T]

object PrimitiveBase:
  sealed trait Read[+A] extends Product, Serializable:
    def constraints: Chain[Constraint.Primitive]

    def map[T](f: A => T): PrimitiveBase.Read[T]

  object Read:
    given Functor[PrimitiveBase.Read] with
      final override def map[A, B](fa: PrimitiveBase.Read[A])(f: A => B): PrimitiveBase.Read[B] = fa.map(f)

    given Primitive.Read[PrimitiveBase.Read] = new Primitive.Read[PrimitiveBase.Read] {}

  sealed trait Write[-A] extends Product, Serializable:
    def contramap[T](f: T => A): PrimitiveBase.Write[T]

  object Write:
    given Contravariant[PrimitiveBase.Write] with
      final override def contramap[A, B](fa: PrimitiveBase.Write[A])(f: B => A): PrimitiveBase.Write[B] =
        fa.contramap(f)

    given Primitive.Write[PrimitiveBase.Write] = new Primitive.Write[PrimitiveBase.Write] {}

  sealed abstract class Boolean[A]
      extends PrimitiveBase[A],
        PrimitiveBase.Boolean.Read[A],
        PrimitiveBase.Boolean.Write[A]:
    final override def imap[T](f: A => T)(g: T => A): PrimitiveBase.Boolean[T] = Boolean.Modify(self = this, f, g)

  object Boolean:
    sealed trait Read[+A] extends PrimitiveBase.Read[A]:
      override def constraints: Chain[Constraint.Primitive] = Chain.empty

      final override def map[T](f: A => T): PrimitiveBase.Boolean.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: PrimitiveBase.Boolean.Read[A], f: A => B)
          extends PrimitiveBase.Boolean.Read[B]

      case object Root extends PrimitiveBase.Boolean.Read[SBoolean]

      given Functor[PrimitiveBase.Boolean.Read] with
        final override def map[A, B](fa: PrimitiveBase.Boolean.Read[A])(f: A => B): PrimitiveBase.Boolean.Read[B] =
          fa.map(f)

      given Primitive.Boolean.Read[PrimitiveBase.Boolean.Read] with
        override def boolean: PrimitiveBase.Boolean.Read[SBoolean] = Root

    sealed trait Write[-A] extends PrimitiveBase.Write[A]:
      final override def contramap[T](f: T => A): PrimitiveBase.Boolean.Write[T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: PrimitiveBase.Boolean.Write[A], f: B => A)
          extends PrimitiveBase.Boolean.Write[B]

      case object Root extends PrimitiveBase.Boolean.Write[SBoolean]

      given Contravariant[PrimitiveBase.Boolean.Write] with
        final override def contramap[A, B](fa: PrimitiveBase.Boolean.Write[A])(
            f: B => A
        ): PrimitiveBase.Boolean.Write[B] =
          fa.contramap(f)

      given Primitive.Boolean.Write[PrimitiveBase.Boolean.Write] with
        override def boolean: PrimitiveBase.Boolean.Write[SBoolean] = Root

    final case class Modify[A, B](self: PrimitiveBase.Boolean[A], f: A => B, g: B => A) extends PrimitiveBase.Boolean[B]

    case object Root extends PrimitiveBase.Boolean[SBoolean]

    given Invariant[PrimitiveBase.Boolean] with
      final override def imap[A, B](fa: PrimitiveBase.Boolean[A])(f: A => B)(g: B => A): PrimitiveBase.Boolean[B] =
        fa.imap(f)(g)

    given Primitive.Boolean[PrimitiveBase.Boolean] with
      override def boolean: Boolean[SBoolean] = Root

  sealed abstract class Number[A] extends PrimitiveBase[A], PrimitiveBase.Number.Read[A], PrimitiveBase.Number.Write[A]:
    final override def imap[T](f: A => T)(g: T => A): PrimitiveBase.Number[T] = Number.Modify(self = this, f, g)

  object Number:
    sealed trait Read[+A] extends PrimitiveBase.Read[A]:
      def constraints: Chain[Constraint.Primitive.Number]

      final override def map[T](f: A => T): PrimitiveBase.Number.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class BigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal])
          extends PrimitiveBase.Number.Read[JBigDecimal]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
          extends PrimitiveBase.Number.Read[JBigInteger]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
          extends PrimitiveBase.Number.Read[SDouble]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat])
          extends PrimitiveBase.Number.Read[SFloat]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Int(validation: Validation[Constraint.Primitive.Number, SInt])
          extends PrimitiveBase.Number.Read[SInt]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Long(validation: Validation[Constraint.Primitive.Number, SLong])
          extends PrimitiveBase.Number.Read[SLong]:
        override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

      final case class Modify[A, B](self: PrimitiveBase.Number.Read[A], f: A => B) extends PrimitiveBase.Number.Read[B]:
        export self.constraints

      given Functor[PrimitiveBase.Number.Read] with
        final override def map[A, B](fa: PrimitiveBase.Number.Read[A])(f: A => B): PrimitiveBase.Number.Read[B] =
          fa.map(f)

      given Primitive.Number.Read[PrimitiveBase.Number.Read] with
        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): PrimitiveBase.Number.Read[JBigDecimal] = BigDecimal(validation)

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): PrimitiveBase.Number.Read[JBigInteger] = BigInteger(validation)

        override def constraints[A](self: PrimitiveBase.Number.Read[A]): Chain[Constraint.Primitive.Number] =
          self.constraints

        override def double(
            validation: Validation[Constraint.Primitive.Number, SDouble]
        ): PrimitiveBase.Number.Read[SDouble] = Double(validation)

        override def float(
            validation: Validation[Constraint.Primitive.Number, SFloat]
        ): PrimitiveBase.Number.Read[SFloat] = Float(validation)

        override def int(validation: Validation[Constraint.Primitive.Number, SInt]): PrimitiveBase.Number.Read[SInt] =
          Int(validation)

        override def long(
            validation: Validation[Constraint.Primitive.Number, SLong]
        ): PrimitiveBase.Number.Read[SLong] = Long(validation)

    sealed trait Write[-A] extends PrimitiveBase.Write[A]:
      final override def contramap[T](f: T => A): PrimitiveBase.Number.Write[T] = Write.Modify(self = this, f)

    object Write:
      case object BigDecimal extends PrimitiveBase.Number.Write[JBigDecimal]

      case object BigInteger extends PrimitiveBase.Number.Write[JBigInteger]

      case object Double extends PrimitiveBase.Number.Write[SDouble]

      case object Float extends PrimitiveBase.Number.Write[SFloat]

      case object Int extends PrimitiveBase.Number.Write[SInt]

      case object Long extends PrimitiveBase.Number.Write[SLong]

      final case class Modify[A, B](self: PrimitiveBase.Number.Write[A], f: B => A)
          extends PrimitiveBase.Number.Write[B]

      given Contravariant[PrimitiveBase.Number.Write] with
        final override def contramap[A, B](fa: PrimitiveBase.Number.Write[A])(
            f: B => A
        ): PrimitiveBase.Number.Write[B] = fa.contramap(f)

      given Primitive.Number.Write[PrimitiveBase.Number.Write] with
        override def bigDecimal: PrimitiveBase.Number.Write[JBigDecimal] = BigDecimal

        override def bigInteger: PrimitiveBase.Number.Write[JBigInteger] = BigInteger

        override def double: PrimitiveBase.Number.Write[SDouble] = Double

        override def float: PrimitiveBase.Number.Write[SFloat] = Float

        override def int: PrimitiveBase.Number.Write[SInt] = Int

        override def long: PrimitiveBase.Number.Write[SLong] = Long

    final case class BigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal])
        extends PrimitiveBase.Number[JBigDecimal]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
        extends PrimitiveBase.Number[JBigInteger]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
        extends PrimitiveBase.Number[SDouble]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat])
        extends PrimitiveBase.Number[SFloat]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Int(validation: Validation[Constraint.Primitive.Number, SInt]) extends PrimitiveBase.Number[SInt]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Long(validation: Validation[Constraint.Primitive.Number, SLong])
        extends PrimitiveBase.Number[SLong]:
      override def constraints: Chain[Constraint.Primitive.Number] = validation.constraints

    final case class Modify[A, B](self: PrimitiveBase.Number[A], f: A => B, g: B => A) extends PrimitiveBase.Number[B]:
      export self.constraints

    given Invariant[PrimitiveBase.Number] with
      final override def imap[A, B](fa: PrimitiveBase.Number[A])(f: A => B)(g: B => A): PrimitiveBase.Number[B] =
        fa.imap(f)(g)

    given Primitive.Number[PrimitiveBase.Number] with
      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): PrimitiveBase.Number[JBigDecimal] =
        BigDecimal(validation)

      override def bigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): PrimitiveBase.Number[JBigInteger] =
        BigInteger(validation)

      override def constraints[A](self: PrimitiveBase.Number[A]): Chain[Constraint.Primitive.Number] = self.constraints

      override def double(validation: Validation[Constraint.Primitive.Number, SDouble]): PrimitiveBase.Number[SDouble] =
        Double(validation)

      override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): PrimitiveBase.Number[SFloat] =
        Float(validation)

      override def int(validation: Validation[Constraint.Primitive.Number, SInt]): PrimitiveBase.Number[SInt] =
        Int(validation)

      override def long(validation: Validation[Constraint.Primitive.Number, SLong]): PrimitiveBase.Number[SLong] =
        Long(validation)

  sealed abstract class Text[A] extends PrimitiveBase[A], PrimitiveBase.Text.Read[A], PrimitiveBase.Text.Write[A]:
    final override def imap[T](f: A => T)(g: T => A): PrimitiveBase.Text[T] = Text.Modify(self = this, f, g)

  object Text:
    sealed trait Read[+A] extends PrimitiveBase.Read[A]:
      def constraints: Chain[Constraint.Primitive.Text]

      final override def map[T](f: A => T): PrimitiveBase.Text.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: PrimitiveBase.Text.Read[A], f: A => B) extends PrimitiveBase.Text.Read[B]:
        export self.constraints

      final case class Parser[A](name: JString, parse: JString => Either[JString, A])
          extends PrimitiveBase.Text.Read[A]:
        override def constraints: Chain[Constraint.Primitive.Text] = Chain.empty

      final case class Root(validation: Validation[Constraint.Primitive.Text, JString])
          extends PrimitiveBase.Text.Read[JString]:
        override def constraints: Chain[Constraint.Primitive.Text] = validation.constraints

      given Functor[PrimitiveBase.Text.Read] with
        final override def map[A, B](fa: PrimitiveBase.Text.Read[A])(f: A => B): PrimitiveBase.Text.Read[B] =
          fa.map(f)

      given Primitive.Text.Read[PrimitiveBase.Text.Read] with
        override def constraints[A](self: PrimitiveBase.Text.Read[A]): Chain[Constraint.Primitive.Text] =
          self.constraints

        override def parser[A](name: JString, parse: JString => Either[JString, A]): PrimitiveBase.Text.Read[A] =
          Parser(name, parse)

        override def string(
            validation: Validation[Constraint.Primitive.Text, JString]
        ): PrimitiveBase.Text.Read[JString] =
          Root(validation)

    sealed trait Write[-A] extends PrimitiveBase.Write[A]:
      final override def contramap[T](f: T => A): PrimitiveBase.Text.Write[T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: PrimitiveBase.Text.Write[A], f: B => A) extends PrimitiveBase.Text.Write[B]

      final case class Printer[A](name: JString, print: A => JString) extends PrimitiveBase.Text.Write[A]

      case object Root extends PrimitiveBase.Text.Write[JString]

      given Contravariant[PrimitiveBase.Text.Write] with
        final override def contramap[A, B](fa: PrimitiveBase.Text.Write[A])(f: B => A): PrimitiveBase.Text.Write[B] =
          fa.contramap(f)

      given Primitive.Text.Write[PrimitiveBase.Text.Write] with
        override def printer[A](name: JString, print: A => JString): PrimitiveBase.Text.Write[A] = Printer(name, print)

        override def string: PrimitiveBase.Text.Write[JString] = Root

    final case class Codec[A](name: JString, parse: JString => Either[JString, A], print: A => JString)
        extends PrimitiveBase.Text[A]:
      override def constraints: Chain[Constraint.Primitive.Text] = Chain.empty

    final case class Modify[A, B](self: PrimitiveBase.Text[A], f: A => B, g: B => A) extends PrimitiveBase.Text[B]:
      export self.constraints

    final case class Root(validation: Validation[Constraint.Primitive.Text, JString])
        extends PrimitiveBase.Text[JString]:
      override def constraints: Chain[Constraint.Primitive.Text] = validation.constraints

    given Invariant[PrimitiveBase.Text] with
      final override def imap[A, B](fa: PrimitiveBase.Text[A])(f: A => B)(g: B => A): PrimitiveBase.Text[B] =
        fa.imap(f)(g)

    given Primitive.Text[PrimitiveBase.Text] with
      override def codec[A](
          name: JString,
          parse: JString => Either[JString, A],
          print: A => JString
      ): PrimitiveBase.Text[A] = Codec(name, parse, print)

      override def constraints[A](self: PrimitiveBase.Text[A]): Chain[Constraint.Primitive.Text] = self.constraints

      override def string(validation: Validation[Constraint.Primitive.Text, JString]): PrimitiveBase.Text[JString] =
        Root(validation)

  given Invariant[PrimitiveBase] with
    final override def imap[A, B](fa: PrimitiveBase[A])(f: A => B)(g: B => A): PrimitiveBase[B] = fa.imap(f)(g)

  given Primitive[PrimitiveBase] = new Primitive[PrimitiveBase] {}
