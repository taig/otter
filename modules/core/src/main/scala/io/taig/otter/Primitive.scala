package io.taig.otter

import cats.arrow.Profunctor
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

/** Leaf of a schema. `W` is the type it writes, `R` the type it reads. */
sealed trait Primitive[-W, +R]

object Primitive:
  final case class Modify[W0, R0, W, R](self: Primitive[W0, R0], f: R0 => R, g: W => W0) extends Primitive[W, R]

  given Profunctor[Primitive]:
    override def dimap[W0, R0, W, R](self: Primitive[W0, R0])(f: W => W0)(g: R0 => R): Primitive[W, R] =
      Modify(self, g, f)

  sealed trait Boolean[-W, +R] extends Primitive[W, R]

  object Boolean:
    case object Root extends Primitive.Boolean[SBoolean, SBoolean]

    final case class Modify[W0, R0, W, R](self: Primitive.Boolean[W0, R0], f: R0 => R, g: W => W0)
        extends Primitive.Boolean[W, R]

    given Profunctor[Primitive.Boolean]:
      override def dimap[W0, R0, W, R](
          self: Primitive.Boolean[W0, R0]
      )(f: W => W0)(g: R0 => R): Primitive.Boolean[W, R] = Modify(self, g, f)

  sealed trait Number[-W, +R] extends Primitive[W, R]

  object Number:
    final case class BigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal])
        extends Primitive.Number[JBigDecimal, JBigDecimal]

    final case class BigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger])
        extends Primitive.Number[JBigInteger, JBigInteger]

    final case class Double(validation: Validation[Constraint.Primitive.Number, SDouble])
        extends Primitive.Number[SDouble, SDouble]

    final case class Float(validation: Validation[Constraint.Primitive.Number, SFloat])
        extends Primitive.Number[SFloat, SFloat]

    final case class Int(validation: Validation[Constraint.Primitive.Number, SInt]) extends Primitive.Number[SInt, SInt]

    final case class Long(validation: Validation[Constraint.Primitive.Number, SLong])
        extends Primitive.Number[SLong, SLong]

    final case class Modify[W0, R0, W, R](self: Primitive.Number[W0, R0], f: R0 => R, g: W => W0)
        extends Primitive.Number[W, R]

    given Profunctor[Primitive.Number]:
      override def dimap[W0, R0, W, R](
          self: Primitive.Number[W0, R0]
      )(f: W => W0)(g: R0 => R): Primitive.Number[W, R] = Modify(self, g, f)

  sealed trait Text[-W, +R] extends Primitive[W, R]

  object Text:
    final case class Root(validation: Validation[Constraint.Primitive.Text, String])
        extends Primitive.Text[String, String]

    /** A named, partial text codec. Either half may be absent, see [[io.taig.otter.operation.PrimitiveOperation.Text]].
      */
    final case class Codec[W, R](name: String, parse: String => Either[String, R], print: W => String)
        extends Primitive.Text[W, R]

    final case class Modify[W0, R0, W, R](self: Primitive.Text[W0, R0], f: R0 => R, g: W => W0)
        extends Primitive.Text[W, R]

    given Profunctor[Primitive.Text]:
      override def dimap[W0, R0, W, R](
          self: Primitive.Text[W0, R0]
      )(f: W => W0)(g: R0 => R): Primitive.Text[W, R] = Modify(self, g, f)
