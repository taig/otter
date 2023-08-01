package io.taig.crock

import cats.data.Chain
import io.circe.{Json, JsonObject}
import io.circe.syntax.*
import io.taig.crock.schema.*
import io.taig.crock.validation.*

final class OpenApi(encoder: Encoder[Schema, Json]):
  def primitive(schema: Primitive[?]): JsonObject = JsonObject(
    "type" := tpe(schema.tpe),
    "format" := schema.format.value,
    "description" := schema.description.value,
    "example" := schema.example.value.map(encoder.encode(schema, _))
  ).deepMerge(constraints(schema.tpe)(schema.constraints))

  val tpe: Type[?] => String =
    case Type.BigDecimal => "big-decimal"
    case Type.BigInt     => "big-int"
    case Type.Boolean    => "boolean"
    case Type.Double     => "double"
    case Type.Float      => "float"
    case Type.Int        => "integer"
    case Type.Long       => "long"
    case Type.String     => "string"

  def constraints(tpe: Type[?]): Chain[Constraint] => JsonObject =
    _.foldLeft(JsonObject.empty)((result, current) => result.deepMerge(constraint(tpe)(current)))

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

    val string: Constraint => JsonObject =
      case Constraint.MinLength(reference) => JsonObject("minLength" := reference)
      case Constraint.MaxLength(reference) => JsonObject("maxLength" := reference)
      case Constraint.Matches(pattern)     => JsonObject("pattern" := pattern.pattern())
      case _                               => JsonObject.empty
