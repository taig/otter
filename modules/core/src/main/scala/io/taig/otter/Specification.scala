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
    def copy(
        description: Option[JString] = description,
        example: Option[OpenApi] = example,
        name: Option[JString] = name
    ): Specification.Value

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
      override def copy(description: Option[JString], example: Option[OpenApi], name: Option[JString]): Value = ???

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
      override def copy(description: Option[JString], example: Option[OpenApi], name: Option[JString]): Value = ???
      override val tpe: JString = "string"

    final case class Boolean(
        description: Option[JString],
        example: Option[OpenApi],
        format: Option[JString],
        name: Option[JString],
        nullable: Option[SBoolean]
    ) extends Specification.Primitive:
      override def copy(description: Option[JString], example: Option[OpenApi], name: Option[JString]): Value = ???
      override val tpe: JString = "boolean"

  final case class Array(
      description: Option[JString],
      example: Option[OpenApi],
      items: Specification.Schema,
      name: Option[JString],
      minItems: Option[Long],
      maxItems: Option[Long],
      uniqueItems: Option[Boolean]
  ) extends Specification.Value:
    override def copy(description: Option[JString], example: Option[OpenApi], name: Option[JString]): Value = ???

  final case class OneOf(
      branches: NonEmptyChain[Specification.Schema],
      description: Option[JString],
      example: Option[OpenApi],
      name: Option[JString]
  ) extends Specification.Value:
    override def copy(description: Option[JString], example: Option[OpenApi], name: Option[JString]): Value = ???

  final case class Object(
      description: Option[JString],
      example: Option[OpenApi],
      name: Option[JString],
      properties: Chain[(String, Specification.Schema)],
      required: Chain[String]
  ) extends Specification.Value:
    override def copy(description: Option[JString], example: Option[OpenApi], name: Option[JString]): Value = ???
