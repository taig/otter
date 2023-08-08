package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation}

sealed abstract class Primitive[A] extends Schema.Value[A]:
  self =>
  final override type Self[a] = Primitive[a]
  final override type Codec = OpenApi.Primitive
  final override type Properties[a] = Primitive.Properties[a]

  final def format: Property.Optional[String] = Property.Optional(_.format, _.modifyFormat)

  final override def copy(update: Primitive.Properties[A]): Primitive[A] = new Primitive[A]:
    export self.{constraints, decode, encode, isOptional, parse, print}
    override def properties: Primitive.Properties[A] = update

  final override def optional: Primitive[Option[A]] = new Primitive[Option[A]]:
    export self.constraints
    override def properties: Primitive.Properties[Option[A]] = self.properties.map(_.some)
    override def isOptional: Boolean = true
    override def decode(openapi: OpenApi): Validated[Violations, Option[A]] = self
      .decode(openapi)
      .map:
        case OpenApi.Null => none
        case openapi      => openapi.some
    override def encode(a: Option[A]): Option[Codec] = a.flatMap(self.encode)
    override def parse(value: Option[String]): Validated[Violations, Option[A]] = ???
    override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = new Primitive[B]:
    export self.isOptional
    override def properties: Primitive.Properties[B] = self.properties.flatMap(validation(_).toOption)
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def decode(openapi: OpenApi): Validated[Violations, B] =
      self.decode(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[Codec] = self.encode(g(b))
    override def parse(value: Option[String]): Validated[Violations, B] = ???
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
    override def decode(openapi: OpenApi): Validated[Violations, A] = (tpe, openapi) match
      case (Type.BigDecimal, openapi: OpenApi.Decimal) => openapi.toBigDecimal.valid
    override def encode(a: A): Option[OpenApi.Primitive] = tpe match
      case Type.BigDecimal => OpenApi.Decimal(a).some
      case Type.BigInt     => OpenApi.Integer(a).some
      case Type.Boolean    => OpenApi.Bool(a).some
      case Type.Double     => OpenApi.Decimal(a).some
      case Type.Float      => OpenApi.Decimal(a).some
      case Type.Int        => OpenApi.Integer(a).some
      case Type.Long       => OpenApi.Integer(a).some
      case Type.String     => OpenApi.Text(a).some
    override def parse(value: Option[String]): Validated[Violations, A] = ???
    override def print(a: A): Option[String] = ???
