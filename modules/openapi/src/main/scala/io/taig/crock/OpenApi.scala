package io.taig.crock

import cats.data.Chain
import io.circe.{Json, JsonObject}
import io.circe.syntax.*
import io.taig.crock.schema.*
import io.taig.crock.validation.*
import scala.util.chaining.*

import scala.annotation.tailrec

object OpenApi:
  self =>

  val schema: Schema[?] => JsonObject =
    case schema: Primitive[?]   => primitive(schema)
    case schema: Collection[?]  => collection(schema)
    case schema: Enumeration[?] => enumeration(schema)
    case schema: Record[?, ?]   => record(schema)

  def primitive(schema: Primitive[?]): JsonObject =
    val format = schema.format.value.fold(JsonObject.empty)(format => JsonObject("format" := format))
    val description = schema.description.value
      .fold(JsonObject.empty)(description => JsonObject("description" := description))
    constraints(schema.tpe)(schema.constraints)
      .deepMerge(format)
      .deepMerge(description)
      .deepMerge(
        JsonObject(
          "type" := typeOf(schema.tpe),
          "nullable" := schema.isOptional,
          "example" := schema.example.value.map(CirceEncoder.schema.encode(schema, _))
        )
      )

  def collection(schema: Collection[?]): JsonObject = constraints(schema).deepMerge(
    JsonObject(
      "type" := "array",
      "items" := self.schema(schema.of.value),
      "nullable" := schema.isOptional
    )
  )

  def enumeration(schema: Enumeration[?]): JsonObject = JsonObject(
    "type" := typeOf(schema.schema.value),
    "enum" := schema.values(CirceEncoder.value),
    "nullable" := schema.isOptional
  )

  def record(schema: Record[?, ?]): JsonObject =
    val properties = schema.fields.toList.map: field =>
      field.name(StringEncoder.value).getOrElse("") := self.schema(field.schema.value)

    val required = schema.fields
      .filterNot(_.schema.value.isOptional)
      .map(_.name(StringEncoder.value).getOrElse(""))
      .pipe(required => if required.isEmpty then JsonObject.empty else JsonObject("required" := required))

    required.deepMerge(
      JsonObject(
        "type" := "object",
        "properties" := Json.fromFields(properties),
        "nullable" := schema.isOptional
      )
    )

  def constraints(tpe: Type[?]): Chain[Constraint] => JsonObject =
    _.foldLeft(JsonObject.empty)((result, current) => constraint(tpe)(current).deepMerge(result))

  def constraints(schema: Collection[?]): JsonObject =
    schema.constraints.foldLeft(JsonObject.empty)((result, current) => constraint.array(current).deepMerge(result))

  object constraint:
    def apply(tpe: Type[?]): Constraint => JsonObject = constraint =>
      tpe match
        case Type.Int | Type.BigInt | Type.BigDecimal | Type.Double | Type.Float | Type.Long => numeric(constraint)
        case Type.String                                                                     => string(constraint)
        case Type.Boolean                                                                    => JsonObject.empty

    val numeric: Constraint => JsonObject =
      case Constraint.Minimum(reference, exclusive) =>
        JsonObject("minimum" := reference, "exclusiveMinimum" := exclusive)
      case Constraint.Maximum(reference, exclusive) =>
        JsonObject("maximum" := reference, "exclusiveMaximum" := exclusive)
      case Constraint.Multiple(of) => JsonObject("multipleOf" := of)
      case _                       => JsonObject.empty

    val array: Constraint => JsonObject =
      case Constraint.MinItems(reference) => JsonObject("minItems" := reference)
      case Constraint.MaxItems(reference) => JsonObject("maxItems" := reference)
      case Constraint.UniqueItems         => JsonObject("unqiueItems" := true)
      case _                              => JsonObject.empty

    val string: Constraint => JsonObject =
      case Constraint.MinLength(reference) => JsonObject("minLength" := reference)
      case Constraint.MaxLength(reference) => JsonObject("maxLength" := reference)
      case Constraint.Matches(pattern)     => JsonObject("pattern" := pattern.pattern())
      case _                               => JsonObject.empty

  @tailrec
  def typeOf(schema: Schema.Value[?]): String = schema match
    case enumeration: Enumeration[?] => typeOf(enumeration.schema.value)
    case primitive: Primitive[?]     => typeOf(primitive.tpe)

  val typeOf: Type[?] => String =
    case Type.Double | Type.Float | Type.BigDecimal => "number"
    case Type.Int | Type.Long | Type.BigInt         => "integer"
    case Type.Boolean                               => "boolean"
    case Type.String                                => "string"
