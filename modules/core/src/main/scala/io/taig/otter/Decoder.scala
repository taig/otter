package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violation

trait Decoder[A]:
  self =>
  def decode(openapi: OpenApi): Validated[Violation, A]

  final def map[B](f: A => B): Decoder[B] = new Decoder[B]:
    override def decode(openapi: OpenApi): Validated[Violation, B] = self.decode(openapi).map(f)

object Decoder:
  inline def apply[A](using decoder: Decoder[A]): Decoder[A] = decoder

  given Decoder[String] with
    override def decode(openapi: OpenApi): Validated[Violation, String] = openapi match
      case OpenApi.Text(value) => value.valid
      case _                   => Violation.tpe("string", openapi.tpe).invalid

  given Decoder[Boolean] with
    override def decode(openapi: OpenApi): Validated[Violation, Boolean] = openapi match
      case OpenApi.Bool(value) => value.valid
      case _                   => Violation.tpe("boolean", openapi.tpe).invalid

  given Decoder[BigDecimal] with
    override def decode(openapi: OpenApi): Validated[Violation, BigDecimal] = openapi match
      case OpenApi.Integer(value: Int)        => BigDecimal(value).valid
      case OpenApi.Integer(value: Long)       => BigDecimal(value).valid
      case OpenApi.Integer(value: BigInt)     => BigDecimal(value).valid
      case OpenApi.Decimal(value: Float)      => BigDecimal(value).valid
      case OpenApi.Decimal(value: Double)     => BigDecimal(value).valid
      case OpenApi.Decimal(value: BigDecimal) => value.valid
      case OpenApi.Text(value) =>
        Validated
          .catchOnly[NumberFormatException](BigDecimal(value))
          .leftMap(_ => Violation.tpe("bigDecimal", openapi.tpe))
      case _ => Violation.tpe("bigDecimal", openapi.tpe).invalid

  given Decoder[BigInt] with
    override def decode(openapi: OpenApi): Validated[Violation, BigInt] = openapi match
      case OpenApi.Integer(value: Int)    => BigInt(value).valid
      case OpenApi.Integer(value: Long)   => BigInt(value).valid
      case OpenApi.Integer(value: BigInt) => value.valid
      case OpenApi.Decimal(value: Float) =>
        Validated.cond(value.isWhole, BigInt(value.toInt), Violation.tpe("bigInt", openapi.tpe))
      case OpenApi.Decimal(value: Double) =>
        Validated.cond(value.isWhole, BigInt(value.toLong), Violation.tpe("bigInt", openapi.tpe))
      case OpenApi.Decimal(value: BigDecimal) => value.toBigIntExact.toValid(Violation.tpe("bigInt", openapi.tpe))
      case OpenApi.Text(value) =>
        Validated.catchOnly[NumberFormatException](BigInt(value)).leftMap(_ => Violation.tpe("bigInt", openapi.tpe))
      case _ => Violation.tpe("bigInt", openapi.tpe).invalid

  given Decoder[Float] with
    override def decode(openapi: OpenApi): Validated[Violation, Float] = openapi match
      case OpenApi.Decimal(value: BigDecimal) => value.toFloat.valid
      case OpenApi.Decimal(value: Double) =>
        Validated.cond(
          value >= Float.MinValue && value <= Float.MaxValue,
          value.toFloat,
          Violation.tpe("float", openapi.tpe)
        )
      case OpenApi.Decimal(value: Float) => value.valid
      case OpenApi.Integer(value: BigInt) =>
        Validated.cond(value.isValidFloat, value.toFloat, Violation.tpe("float", openapi.tpe))
      case OpenApi.Integer(value: Int) => value.toFloat.valid
      case OpenApi.Integer(value: Long) =>
        Validated.cond(
          value >= Float.MinValue && value <= Float.MaxValue,
          value.toFloat,
          Violation.tpe("float", openapi.tpe)
        )
      case OpenApi.Text(value) => value.toFloatOption.toValid(Violation.tpe("float", openapi.tpe))
      case _                   => Violation.tpe("float", openapi.tpe).invalid

  given Decoder[Double] with
    override def decode(openapi: OpenApi): Validated[Violation, Double] = openapi match
      case OpenApi.Decimal(value: BigDecimal) => value.toDouble.valid
      case OpenApi.Decimal(value: Double)     => value.valid
      case OpenApi.Decimal(value: Float)      => value.toDouble.valid
      case OpenApi.Integer(value: BigInt)     => value.toDouble.valid
      case OpenApi.Integer(value: Int)        => value.toDouble.valid
      case OpenApi.Integer(value: Long)       => value.toDouble.valid
      case OpenApi.Text(value)                => value.toDoubleOption.toValid(Violation.tpe("double", openapi.tpe))
      case _                                  => Violation.tpe("double", openapi.tpe).invalid

  given Decoder[Int] with
    override def decode(openapi: OpenApi): Validated[Violation, Int] = openapi match
      case OpenApi.Decimal(value: BigDecimal) =>
        Validated.cond(value.isValidInt, value.toInt, Violation.tpe("int", openapi.tpe))
      case OpenApi.Decimal(value: Double) =>
        Validated.cond(value.isValidInt, value.toInt, Violation.tpe("int", openapi.tpe))
      case OpenApi.Decimal(value: Float) =>
        Validated.cond(value.isValidInt, value.toInt, Violation.tpe("int", openapi.tpe))
      case OpenApi.Integer(value: BigInt) =>
        Validated.cond(value.isValidInt, value.toInt, Violation.tpe("int", openapi.tpe))
      case OpenApi.Integer(value: Int) => value.valid
      case OpenApi.Integer(value: Long) =>
        Validated.cond(value.isValidInt, value.toInt, Violation.tpe("int", openapi.tpe))
      case OpenApi.Text(value) => value.toIntOption.toValid(Violation.tpe("int", openapi.tpe))
      case _                   => Violation.tpe("int", openapi.tpe).invalid

  given Decoder[Long] with
    override def decode(openapi: OpenApi): Validated[Violation, Long] = openapi match
      case OpenApi.Decimal(value: BigDecimal) =>
        Validated.cond(value.isValidLong, value.toLong, Violation.tpe("long", openapi.tpe))
      case OpenApi.Decimal(value: Double) =>
        Validated.cond(
          value.isWhole && value >= Long.MinValue && value <= Long.MaxValue,
          value.toLong,
          Violation.tpe("long", openapi.tpe)
        )
      case OpenApi.Decimal(value: Float) =>
        Validated.cond(value.isWhole, value.toLong, Violation.tpe("long", openapi.tpe))
      case OpenApi.Integer(value: BigInt) =>
        Validated.cond(value.isValidLong, value.toLong, Violation.tpe("long", openapi.tpe))
      case OpenApi.Integer(value: Int)  => value.toLong.valid
      case OpenApi.Integer(value: Long) => value.valid
      case OpenApi.Text(value)          => value.toLongOption.toValid(Violation.tpe("long", openapi.tpe))
      case _                            => Violation.tpe("long", openapi.tpe).invalid
