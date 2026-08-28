package io.taig.otter.operation

import io.taig.otter.Constraint
import io.taig.validation.Validation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

object PrimitiveOperation:
  trait Boolean[F[- _, + _]]:
    def boolean: F[SBoolean, SBoolean]

  trait Number[F[- _, + _]]:
    def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal, JBigDecimal]
    def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger, JBigInteger]
    def double(validation: Validation[Constraint.Primitive.Number, SDouble]): F[SDouble, SDouble]
    def float(validation: Validation[Constraint.Primitive.Number, SFloat]): F[SFloat, SFloat]
    def int(validation: Validation[Constraint.Primitive.Number, SInt]): F[SInt, SInt]
    def long(validation: Validation[Constraint.Primitive.Number, SLong]): F[SLong, SLong]

  trait Text[F[- _, + _]]:
    def string(validation: Validation[Constraint.Primitive.Text, String]): F[String, String]

    /** A named, partial text codec. Instantiate `W` or `R` to [[io.taig.otter.Void]] for a one directional schema. */
    def codec[W, R](name: String, parse: String => Either[String, R], print: W => String): F[W, R]
