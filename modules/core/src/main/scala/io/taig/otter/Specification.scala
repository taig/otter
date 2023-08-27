package io.taig.otter

import cats.data.{Chain, NonEmptyChain}

import java.util.regex.Pattern
import java.lang.String as JString
import scala.Boolean as SBoolean

final case class Specification()

object Specification:
  sealed abstract class Schema extends Product with Serializable

  final case class Reference(path: String) extends Schema

  sealed abstract class Value extends Schema:
    def description: Option[JString]
    def example: Option[OpenApi]
    def name: Option[JString]

    def modifyDescription(f: Option[JString] => Option[JString]): Specification.Value
    def modifyExample(f: Option[OpenApi] => Option[OpenApi]): Specification.Value

  sealed abstract class Primitive extends Specification.Value:
    def format: Option[JString]
    def nullable: Option[SBoolean]
    def tpe: JString

  object Primitive:
    final case class Number(
        description: Option[JString],
        example: Option[OpenApi],
        format: Option[JString],
        name: Option[JString],
        nullable: Option[SBoolean],
        minimum: Option[BigInt],
        exclusiveMinimum: Option[SBoolean],
        maximum: Option[BigInt],
        exclusiveMaximum: Option[SBoolean],
        multipleOf: Option[BigDecimal]
    ) extends Specification.Primitive:
      override val tpe: JString = "number"
      override def modifyDescription(f: Option[JString] => Option[JString]): Specification.Primitive.Number =
        copy(description = f(description))
      override def modifyExample(f: Option[OpenApi] => Option[OpenApi]): Specification.Primitive.Number =
        copy(example = f(example))

    final case class String(
        description: Option[JString],
        example: Option[OpenApi],
        format: Option[JString],
        name: Option[JString],
        nullable: Option[SBoolean],
        minLength: Option[Int],
        maxLength: Option[Int],
        pattern: Option[Pattern]
    ) extends Specification.Primitive:
      override val tpe: JString = "string"
      override def modifyDescription(f: Option[JString] => Option[JString]): Specification.Primitive.String =
        copy(description = f(description))
      override def modifyExample(f: Option[OpenApi] => Option[OpenApi]): Specification.Primitive.String =
        copy(example = f(example))

    final case class Boolean(
        description: Option[JString],
        example: Option[OpenApi],
        format: Option[JString],
        name: Option[JString],
        nullable: Option[SBoolean]
    ) extends Specification.Primitive:
      override val tpe: JString = "boolean"
      override def modifyDescription(f: Option[JString] => Option[JString]): Specification.Primitive.Boolean =
        copy(description = f(description))
      override def modifyExample(f: Option[OpenApi] => Option[OpenApi]): Specification.Primitive.Boolean =
        copy(example = f(example))

  final case class Array(
      description: Option[JString],
      example: Option[OpenApi],
      items: Specification.Schema,
      name: Option[JString],
      minItems: Option[Long],
      maxItems: Option[Long],
      uniqueItems: Option[Boolean]
  ) extends Specification.Value:
    override def modifyDescription(f: Option[JString] => Option[JString]): Specification.Array =
      copy(description = f(description))
    override def modifyExample(f: Option[OpenApi] => Option[OpenApi]): Specification.Array =
      copy(example = f(example))

  final case class OneOf(
      branches: NonEmptyChain[Specification.Schema],
      description: Option[JString],
      example: Option[OpenApi],
      name: Option[JString]
  ) extends Specification.Value:
    override def modifyDescription(f: Option[JString] => Option[JString]): Specification.OneOf =
      copy(description = f(description))
    override def modifyExample(f: Option[OpenApi] => Option[OpenApi]): Specification.OneOf =
      copy(example = f(example))

  final case class Object(
      description: Option[JString],
      example: Option[OpenApi],
      name: Option[JString],
      properties: Chain[(String, Specification.Schema)],
      required: Chain[String]
  ) extends Specification.Value:
    override def modifyDescription(f: Option[JString] => Option[JString]): Specification.Object =
      copy(description = f(description))
    override def modifyExample(f: Option[OpenApi] => Option[OpenApi]): Specification.Object =
      copy(example = f(example))
