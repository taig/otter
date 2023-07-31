package io.taig.openapi.generator

import cats.data.Chain
import io.taig.openapi.OpenApi
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Primitive, Type}
import io.taig.openapi.validation.Constraint

object OpenApiGenerator {
  def primitive(schema: Primitive[?]): OpenApi = OpenApi.obj(
    "type" := tpe(schema.tpe),
    "format" := schema.format.value,
    "description" := schema.description.value,
    "example" := schema.example.encode
  ) ++ constraints(schema.tpe)(schema.constraints)

  val tpe: Type[?] => String =
    case Type.BigDecimal => "big-decimal"
    case Type.BigInt     => "big-int"
    case Type.Boolean    => "boolean"
    case Type.Double     => "double"
    case Type.Float      => "float"
    case Type.Int        => "integer"
    case Type.Long       => "long"
    case Type.String     => "string"

  def constraints(tpe: Type[?]): Chain[Constraint] => OpenApi.Object =
    _.foldLeft(OpenApi.Object.Empty)(_ ++ constraint(tpe)(_))

  object constraint:
    def apply(tpe: Type[?]): Constraint => OpenApi.Object = constraint =>
      tpe match
        case Type.Int | Type.BigInt | Type.BigDecimal | Type.Double | Type.Float | Type.Long => numeric(constraint)
        case Type.String                                                                     => string(constraint)
        case Type.Boolean                                                                    => OpenApi.Object.Empty

    val numeric: Constraint => OpenApi.Object =
      case Constraint.Minimum(reference, exclusive) =>
        OpenApi.obj("minimum" := reference, "exclusiveMinimum" := exclusive)
      case Constraint.Maximum(reference, exclusive) =>
        OpenApi.obj("maximum" := reference, "exclusiveMaximum" := exclusive)
      case Constraint.Multiple(of) => OpenApi.obj("multipleOf" := of)
      case _                       => OpenApi.Object.Empty

    val string: Constraint => OpenApi.Object =
      case Constraint.MinLength(reference) => OpenApi.obj("minLength" := reference)
      case Constraint.MaxLength(reference) => OpenApi.obj("maxLength" := reference)
      case Constraint.Matches(pattern)     => OpenApi.obj("pattern" := pattern.pattern())
      case _                               => OpenApi.Object.Empty
}
