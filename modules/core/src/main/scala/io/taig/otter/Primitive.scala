package io.taig.otter

import cats.data.Chain
import io.taig.otter.Annotation
import io.taig.otter as Self
import io.taig.otter.operation.PrimitiveSchemaInvariant
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
  def constraints: Chain[Constraint.Primitive]

  def imap[T](f: A => T)(g: T => A): Primitive[T]

object Primitive:
  sealed abstract class Boolean[A] extends Primitive[A]:
    final override def constraints: Chain[Constraint.Primitive] = Chain.empty
    final override def imap[T](f: A => T)(g: T => A): Primitive.Boolean[T] = Boolean.Modify(self = this, f, g)

  object Boolean:
    final case class Modify[A, B](self: Primitive.Boolean[A], f: A => B, g: B => A) extends Primitive.Boolean[B]

    case object Root extends Primitive.Boolean[SBoolean]

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
