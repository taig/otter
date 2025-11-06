package io.taig.otter

import cats.Invariant
import cats.data.Chain
import io.taig.otter.operation.*
import io.taig.validation.Constraint
import io.taig.validation.Validation

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong
import cats.Contravariant
import cats.Functor
import io.taig.validation.Constraint.Primitive.Text

sealed abstract class Primitive[A] extends Primitive.Read[A], Primitive.Write[A]:
  def imap[T](f: A => T)(g: T => A): Primitive[T]

object Primitive:
  sealed trait Read[+A] extends Product, Serializable:
    def constraints: Chain[Constraint.Primitive.Number | Constraint.Primitive.Text]

    def map[T](f: A => T): Primitive.Read[T]

  object Read:
    given Functor[Primitive.Read] with
      override def map[A, B](fa: Primitive.Read[A])(f: A => B): Primitive.Read[B] = fa.map(f)

  sealed trait Write[-A] extends Product, Serializable:
    def contramap[T](f: T => A): Primitive.Write[T]

  object Write:
    given Contravariant[Primitive.Write] with
      override def contramap[A, B](fa: Primitive.Write[A])(f: B => A): Primitive.Write[B] = fa.contramap(f)

  final case class Boolean[A](asReader: Primitive.Boolean.Read[A], asWriter: Primitive.Boolean.Write[A])
      extends Primitive[A]:
    export asReader.{constraints, map}
    export asWriter.contramap

    override def imap[T](f: A => T)(g: T => A): Primitive.Boolean[T] =
      copy(asReader = asReader.map(f), asWriter = asWriter.contramap(g))

  object Boolean:
    sealed abstract class Read[+A] extends Primitive.Read[A]:
      final override def constraints: Chain[Constraint.Primitive.Number | Constraint.Primitive.Text] = Chain.empty

      override def map[T](f: A => T): Primitive.Boolean.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.Boolean.Read[A], f: A => B) extends Primitive.Boolean.Read[B]

      case object Root extends Primitive.Boolean.Read[SBoolean]

      given Functor[Primitive.Boolean.Read] with
        override def map[A, B](fa: Primitive.Boolean.Read[A])(f: A => B): Primitive.Boolean.Read[B] = fa.map(f)

      given BooleanOperation.Read[Primitive.Boolean.Read] with
        override def boolean: Primitive.Boolean.Read[SBoolean] = Primitive.Boolean.Read.Root

    sealed abstract class Write[-A] extends Primitive.Write[A]:
      override def contramap[T](f: T => A): Primitive.Boolean.Write[T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.Boolean.Write[A], f: B => A) extends Primitive.Boolean.Write[B]

      case object Root extends Primitive.Boolean.Write[SBoolean]

      given Contravariant[Primitive.Boolean.Write] with
        override def contramap[A, B](fa: Primitive.Boolean.Write[A])(f: B => A): Primitive.Boolean.Write[B] =
          fa.contramap(f)

      given BooleanOperation.Write[Primitive.Boolean.Write] with
        override def boolean: Primitive.Boolean.Write[SBoolean] = Primitive.Boolean.Write.Root

    given Invariant[Primitive.Boolean] with
      override def imap[A, B](fa: Primitive.Boolean[A])(f: A => B)(g: B => A): Primitive.Boolean[B] = fa.imap(f)(g)

    given BooleanOperation[Primitive.Boolean] with
      override def boolean: Primitive.Boolean[SBoolean] =
        Primitive.Boolean(asReader = Primitive.Boolean.Read.Root, asWriter = Primitive.Boolean.Write.Root)

  given Invariant[Primitive] with
    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)

  final case class Number[A](asReader: Primitive.Number.Read[A], asWriter: Primitive.Number.Write[A])
      extends Primitive[A]:
    export asReader.{constraints, map}
    export asWriter.contramap

    override def imap[T](f: A => T)(g: T => A): Primitive.Number[T] =
      copy(asReader = asReader.map(f), asWriter = asWriter.contramap(g))

  object Number:
    sealed abstract class Read[+A] extends Primitive.Read[A]:
      override def constraints: Chain[Constraint.Primitive.Number]

      override def map[T](f: A => T): Primitive.Number.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.Number.Read[A], f: A => B) extends Primitive.Number.Read[B]:
        export self.constraints

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

      given Functor[Primitive.Number.Read] with
        override def map[A, B](fa: Primitive.Number.Read[A])(f: A => B): Primitive.Number.Read[B] = fa.map(f)

      given NumberOperation.Read[Primitive.Number.Read] with
        override def constraints[A](self: Primitive.Number.Read[A]): Chain[Constraint.Primitive.Number] =
          self.constraints

        override def bigDecimal(
            validation: Validation[Constraint.Primitive.Number, JBigDecimal]
        ): Primitive.Number.Read[JBigDecimal] = Primitive.Number.Read.BigDecimal(validation)

        override def bigInteger(
            validation: Validation[Constraint.Primitive.Number, JBigInteger]
        ): Primitive.Number.Read[JBigInteger] = Primitive.Number.Read.BigInteger(validation)

        override def double(
            validation: Validation[Constraint.Primitive.Number, SDouble]
        ): Primitive.Number.Read[SDouble] = Primitive.Number.Read.Double(validation)

        override def float(
            validation: Validation[Constraint.Primitive.Number, SFloat]
        ): Primitive.Number.Read[SFloat] = Primitive.Number.Read.Float(validation)

        override def int(
            validation: Validation[Constraint.Primitive.Number, SInt]
        ): Primitive.Number.Read[SInt] = Primitive.Number.Read.Int(validation)

        override def long(
            validation: Validation[Constraint.Primitive.Number, SLong]
        ): Primitive.Number.Read[SLong] = Primitive.Number.Read.Long(validation)

    sealed abstract class Write[-A] extends Primitive.Write[A]:
      override def contramap[T](f: T => A): Primitive.Number.Write[T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.Number.Write[A], f: B => A) extends Primitive.Number.Write[B]

      case object BigDecimal extends Primitive.Number.Write[JBigDecimal]

      case object BigInteger extends Primitive.Number.Write[JBigInteger]

      case object Double extends Primitive.Number.Write[SDouble]

      case object Float extends Primitive.Number.Write[SFloat]

      case object Int extends Primitive.Number.Write[SInt]

      case object Long extends Primitive.Number.Write[SLong]

      given Contravariant[Primitive.Number.Write] with
        override def contramap[A, B](fa: Primitive.Number.Write[A])(f: B => A): Primitive.Number.Write[B] =
          fa.contramap(f)

      given NumberOperation.Write[Primitive.Number.Write] with
        override def bigDecimal: Primitive.Number.Write[JBigDecimal] = Primitive.Number.Write.BigDecimal

        override def bigInteger: Primitive.Number.Write[JBigInteger] = Primitive.Number.Write.BigInteger

        override def double: Primitive.Number.Write[SDouble] = Primitive.Number.Write.Double

        override def float: Primitive.Number.Write[SFloat] = Primitive.Number.Write.Float

        override def int: Primitive.Number.Write[SInt] = Primitive.Number.Write.Int

        override def long: Primitive.Number.Write[SLong] = Primitive.Number.Write.Long

    given Invariant[Primitive.Number] with
      override def imap[A, B](fa: Number[A])(f: A => B)(g: B => A): Primitive.Number[B] = fa.imap(f)(g)

    given NumberOperation[Primitive.Number] with
      override def constraints[A](self: Primitive.Number[A]): Chain[Constraint.Primitive.Number] =
        self.constraints

      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): Primitive.Number[JBigDecimal] = Primitive.Number(
        asReader = Primitive.Number.Read.BigDecimal(validation),
        asWriter = Primitive.Number.Write.BigDecimal
      )

      override def bigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): Primitive.Number[JBigInteger] = Primitive.Number(
        asReader = Primitive.Number.Read.BigInteger(validation),
        asWriter = Primitive.Number.Write.BigInteger
      )

      override def double(
          validation: Validation[Constraint.Primitive.Number, SDouble]
      ): Primitive.Number[SDouble] = Primitive.Number(
        asReader = Primitive.Number.Read.Double(validation),
        asWriter = Primitive.Number.Write.Double
      )

      override def float(
          validation: Validation[Constraint.Primitive.Number, SFloat]
      ): Primitive.Number[SFloat] = Primitive.Number(
        asReader = Primitive.Number.Read.Float(validation),
        asWriter = Primitive.Number.Write.Float
      )

      override def int(
          validation: Validation[Constraint.Primitive.Number, SInt]
      ): Primitive.Number[SInt] = Primitive.Number(
        asReader = Primitive.Number.Read.Int(validation),
        asWriter = Primitive.Number.Write.Int
      )

      override def long(
          validation: Validation[Constraint.Primitive.Number, SLong]
      ): Primitive.Number[SLong] = Primitive.Number(
        asReader = Primitive.Number.Read.Long(validation),
        asWriter = Primitive.Number.Write.Long
      )

  final case class String[A](asReader: Primitive.String.Read[A], asWriter: Primitive.String.Write[A])
      extends Primitive[A]:
    export asReader.{constraints, map}
    export asWriter.contramap

    override def imap[T](f: A => T)(g: T => A): Primitive.String[T] =
      copy(asReader = asReader.map(f), asWriter = asWriter.contramap(g))

  object String:
    sealed abstract class Read[+A] extends Primitive.Read[A]:
      override def constraints: Chain[Constraint.Primitive.Text]

      override def map[T](f: A => T): Primitive.String.Read[T] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Primitive.String.Read[A], f: A => B) extends Primitive.String.Read[B]:
        export self.constraints

      final case class Parser[A](name: JString, decode: JString => Either[JString, A]) extends Primitive.String.Read[A]:
        override def constraints: Chain[Constraint.Primitive.Text] = Chain.empty

      final case class Root(validation: Validation[Constraint.Primitive.Text, JString])
          extends Primitive.String.Read[JString]:
        override def constraints: Chain[Constraint.Primitive.Text] = validation.constraints

      given Functor[Primitive.String.Read] with
        override def map[A, B](fa: Primitive.String.Read[A])(f: A => B): Primitive.String.Read[B] = fa.map(f)

      given StringOperation[Primitive.String.Read] with
        override def constraints[A](self: Primitive.String.Read[A]): Chain[Constraint.Primitive.Text] =
          self.constraints

        override def codec[A](
            name: JString,
            decode: JString => Either[JString, A],
            encode: A => JString
        ): Primitive.String.Read[A] = Primitive.String.Read.Parser(name, decode)

        override def string(
            validation: Validation[Constraint.Primitive.Text, JString]
        ): Primitive.String.Read[JString] = Primitive.String.Read.Root(validation)

    sealed abstract class Write[-A] extends Primitive.Write[A]:
      override def contramap[T](f: T => A): Primitive.String.Write[T] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Primitive.String.Write[A], f: B => A) extends Primitive.String.Write[B]

      final case class Printer[A](name: JString, encode: A => JString) extends Primitive.String.Write[A]

      case object Root extends Primitive.String.Write[JString]

      given Contravariant[Primitive.String.Write] with
        override def contramap[A, B](fa: Primitive.String.Write[A])(f: B => A): Primitive.String.Write[B] =
          fa.contramap(f)

      given StringOperation.Write[Primitive.String.Write] with
        override def printer[A](name: JString, encode: A => JString): Primitive.String.Write[A] =
          Primitive.String.Write.Printer(name, encode)

        override def string: Primitive.String.Write[JString] = Primitive.String.Write.Root

    given Invariant[Primitive.String] with
      override def imap[A, B](fa: String[A])(f: A => B)(g: B => A): Primitive.String[B] = fa.imap(f)(g)

    given StringOperation[Primitive.String] with
      override def constraints[A](self: Primitive.String[A]): Chain[Constraint.Primitive.Text] =
        self.constraints

      override def codec[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ): Primitive.String[A] = Primitive.String(
        asReader = Primitive.String.Read.Parser(name, decode),
        asWriter = Primitive.String.Write.Printer(name, encode)
      )

      override def string(validation: Validation[Constraint.Primitive.Text, JString]): Primitive.String[JString] =
        Primitive.String(
          asReader = Primitive.String.Read.Root(validation),
          asWriter = Primitive.String.Write.Root
        )
