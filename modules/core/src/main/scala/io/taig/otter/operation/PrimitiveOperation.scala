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
  trait Boolean[F[-_, +_]]:
    def boolean: F[SBoolean, SBoolean]

  trait Number[F[-_, +_]]:
    def bigDecimal(validation: Validation[Constraint.Primitive.Number, JBigDecimal]): F[JBigDecimal, JBigDecimal]
    def bigInteger(validation: Validation[Constraint.Primitive.Number, JBigInteger]): F[JBigInteger, JBigInteger]
    def double(validation: Validation[Constraint.Primitive.Number, SDouble]): F[SDouble, SDouble]
    def float(validation: Validation[Constraint.Primitive.Number, SFloat]): F[SFloat, SFloat]
    def int(validation: Validation[Constraint.Primitive.Number, SInt]): F[SInt, SInt]
    def long(validation: Validation[Constraint.Primitive.Number, SLong]): F[SLong, SLong]

  trait Text[F[-_, +_]]:
    def string(validation: Validation[Constraint.Primitive.Text, String]): F[String, String]

    /** A value carried as text under a named conversion, in the sense of JSON Schema's `format`: `uuid`, `date-time`,
      * `isbn`. The name is what a failure to parse is reported against.
      *
      * Either half may be vacuous. Instantiate `W` to `Nothing` for a schema that can only be read, or `R` to `Any` for
      * one that can only be written. Those are the maximal elements of their slots, so nothing can be encoded through
      * the first and nothing usable decoded out of the second.
      */
    def format[W, R](name: String, parse: String => Either[String, R], print: W => String): F[W, R]
