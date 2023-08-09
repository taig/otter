package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

sealed abstract class Primitive[A] extends Schema.Value[A]:
  self =>
  final override type Self[a] = Primitive[a]
  final override type Codec = OpenApi.Primitive
  final override type Properties[a] = Primitive.Properties[a]

  final def format: Property.Optional[String] = new Property.Optional[String]:
    override def value: Option[String] = properties.format
    override def modify(f: Option[String] => Option[String]): Primitive[A] = copy(properties.modifyFormat(f))

  final override def copy(update: Primitive.Properties[A]): Primitive[A] = new Primitive[A]:
    export self.{constraints, decode, encode, isOptional, parse, print}
    override def properties: Primitive.Properties[A] = update

  final override def optional: Primitive[Option[A]] = new Primitive[Option[A]] with Optional:
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, Option[A]] =
      openapi.traverse(self.decode)
    override def encode(a: Option[A]): Option[Codec] = a.flatMap(self.encode)
    override def parse(value: Option[String]): Validated[Violations, Option[A]] =
      self.parse(value).map(_.some)
    override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = new Primitive[B]
    with Validate[B](validation):
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, B] =
      self.decode(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[Codec] = self.encode(g(b))
    override def parse(value: Option[String]): Validated[Violations, B] =
      self.parse(value).andThen(validation(_).leftMap(Violations.root))
    override def print(b: B): Option[String] = self.print(g(b))

object Primitive:
  final case class Properties[+A](description: Option[String], example: Option[A], format: Option[String])
      extends Schema.Properties[A]:
    override type Self[a] = Primitive.Properties[a]
    override def modifyDescription(f: Option[String] => Option[String]): Primitive.Properties[A] =
      copy(description = f(description))
    override def modifyExample[B](f: Option[A] => Option[B]): Primitive.Properties[B] = copy(example = f(example))
    def modifyFormat(f: Option[String] => Option[String]): Primitive.Properties[A] = copy(format = f(format))
    override def flatMap[B](f: A => Option[B]): Primitive.Properties[B] = copy(example = example.flatMap(f))

  object Properties:
    val Default: Primitive.Properties[Nothing] = Properties(None, None, None)

  def apply[A](tpe: Type[A]): Primitive[A] = new Primitive[A]:
    override def properties: Primitive.Properties[A] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] =
      openapi
        .toValid(Violations.rootNec(Violation.required))
        .andThen(value => decodeType(value).leftMap(Violations.rootNec))
    def decodeType(openapi: OpenApi.Value): Validated[Violation, A] = tpe match
      case Type.BigDecimal => openapi.as[BigDecimal]
      case Type.BigInt     => openapi.as[BigInt]
      case Type.Boolean    => openapi.as[Boolean]
      case Type.Double     => openapi.as[Double]
      case Type.Float      => openapi.as[Float]
      case Type.Int        => openapi.as[Int]
      case Type.Long       => openapi.as[Long]
      case Type.String     => openapi.as[String]
    override def encode(a: A): Option[OpenApi.Primitive] = encodeType(a).some
    def encodeType(a: A): OpenApi.Primitive = tpe match
      case Type.BigDecimal => OpenApi.Decimal(a)
      case Type.BigInt     => OpenApi.Integer(a)
      case Type.Boolean    => OpenApi.Bool(a)
      case Type.Double     => OpenApi.Decimal(a)
      case Type.Float      => OpenApi.Decimal(a)
      case Type.Int        => OpenApi.Integer(a)
      case Type.Long       => OpenApi.Integer(a)
      case Type.String     => OpenApi.Text(a)
    override def parse(value: Option[String]): Validated[Violations, A] =
      value
        .toValid(Violations.rootNec(Violation.required))
        .andThen(value => parseType(value).toValid(Violations.rootNec(Violation.tpe(tpe.toString, value))))
    def parseType(value: String): Option[A] = tpe match
      case Type.BigDecimal =>
        try Some(BigDecimal(value))
        catch case _: NumberFormatException => None
      case Type.BigInt =>
        try Some(BigInt(value))
        catch case _: NumberFormatException => None
      case Type.Boolean => value.toBooleanOption
      case Type.Double  => value.toDoubleOption
      case Type.Float   => value.toFloatOption
      case Type.Int     => value.toIntOption
      case Type.Long    => value.toLongOption
      case Type.String  => Some(value)
    override def print(a: A): Option[String] = Some(printType(a))
    def printType(a: A): String = tpe match
      case Type.BigDecimal => a.toString
      case Type.BigInt     => a.toString
      case Type.Boolean    => String.valueOf(a)
      case Type.Double     => String.valueOf(a)
      case Type.Float      => String.valueOf(a)
      case Type.Int        => String.valueOf(a)
      case Type.Long       => String.valueOf(a)
      case Type.String     => a
