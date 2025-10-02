package io.taig.otter

import cats.data.Chain
import io.taig.otter.operation.BooleanOperation
import io.taig.otter.operation.NumberOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.otter.operation.StringOperation
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

sealed abstract class Primitive[A] extends Product with Serializable:
  def constraints: Chain[Constraint.Primitive.Number | Constraint.Primitive.Text]

  def imap[T](f: A => T)(g: T => A): Primitive[T]

object Primitive:
  sealed abstract class Boolean[A] extends Primitive[A]:
    final override def constraints: Chain[Nothing] = Chain.empty

    final override def imap[T](f: A => T)(g: T => A): Primitive.Boolean[T] = Boolean.Modify(self = this, f, g)

  object Boolean:
    final case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A) extends Primitive.Boolean[B]

    case object Root extends Primitive.Boolean[SBoolean]

    given invariant: Invariant[Primitive.Boolean] with
      extension [A](self: Primitive.Boolean[A])
        def imap[T](f: A => T)(g: T => A): Primitive.Boolean[T] = self.imap(f)(g)

    given operation: BooleanOperation[Primitive.Boolean] with
      override val boolean: Primitive.Boolean[SBoolean] = Primitive.Boolean.Root

  sealed abstract class Number[A] extends Primitive[A]:
    override def constraints: Chain[Constraint.Primitive.Number]
    final override def imap[T](f: A => T)(g: T => A): Primitive.Number[T] = Number.Modify(self = this, f, g)

  object Number:
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

    given invariant: Invariant[Primitive.Number] with
      extension [A](self: Primitive.Number[A]) def imap[T](f: A => T)(g: T => A): Primitive.Number[T] = self.imap(f)(g)

    given operation: NumberOperation[Primitive.Number] with
      override def bigDecimal(
          validation: Validation[Constraint.Primitive.Number, JBigDecimal]
      ): Primitive.Number[JBigDecimal] = BigDecimal(validation)

      override def bigInteger(
          validation: Validation[Constraint.Primitive.Number, JBigInteger]
      ): Primitive.Number[JBigInteger] = BigInteger(validation)

      override def double(validation: Validation[Constraint.Primitive.Number, SDouble]): Primitive.Number[SDouble] =
        Double(validation)

      override def float(validation: Validation[Constraint.Primitive.Number, SFloat]): Primitive.Number[SFloat] =
        Float(validation)

      override def int(validation: Validation[Constraint.Primitive.Number, SInt]): Primitive.Number[SInt] =
        Int(validation)

      override def long(validation: Validation[Constraint.Primitive.Number, SLong]): Primitive.Number[SLong] =
        Long(validation)

      extension [A](self: Primitive.Number[A])
        override def constraints: Chain[Constraint.Primitive.Number] = self.constraints

  sealed abstract class String[A] extends Primitive[A]:
    override def constraints: Chain[Constraint.Primitive.Text]

    final override def imap[T](f: A => T)(g: T => A): Primitive.String[T] = String.Modify(self = this, f, g)

  object String:
    final case class Modify[S[_], A, B](self: Primitive.String[A], f: A => B, g: B => A) extends Primitive.String[B]:
      export self.constraints

    final case class Parser[A](name: JString, decode: JString => Either[JString, A], encode: A => JString)
        extends Primitive.String[A]:
      override def constraints: Chain[Constraint.Primitive.Text] = Chain.empty

    final case class Root(validation: Validation[Constraint.Primitive.Text, JString]) extends Primitive.String[JString]:
      override def constraints: Chain[Constraint.Primitive.Text] = validation.constraints

    given invariant: Invariant[Primitive.String] with
      extension [A](self: Primitive.String[A]) def imap[T](f: A => T)(g: T => A): Primitive.String[T] = self.imap(f)(g)

    given operation: StringOperation[Primitive.String] with
      override def string(validation: Validation[Constraint.Primitive.Text, JString]): Primitive.String[JString] =
        Root(validation)

      override def parser[A](
          name: JString,
          decode: JString => Either[JString, A],
          encode: A => JString
      ): Primitive.String[A] = Parser(name, decode, encode)

      extension [A](self: Primitive.String[A]) override def constraints: Chain[Constraint] = self.constraints

  given invariant: Invariant[Primitive] with
    extension [A](self: Primitive[A])
      def imap[B](f: A => B)(g: B => A): Primitive[B] = self match
        case schema: Primitive.Boolean[A] => schema.imap(f)(g)
        case schema: Primitive.Number[A]  => schema.imap(f)(g)
        case schema: Primitive.String[A]  => schema.imap(f)(g)

  given operation: PrimitiveOperation[Primitive] with
    export Boolean.operation.boolean
    export Number.operation.{bigDecimal, bigInteger, double, float, int, long}
    export String.operation.{parser, string}
    extension [A](self: Primitive[A])
      override def constraints: Chain[Constraint.Primitive.Number | Constraint.Primitive.Text] = self.constraints
