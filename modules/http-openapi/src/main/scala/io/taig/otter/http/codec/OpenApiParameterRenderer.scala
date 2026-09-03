package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter as Self
import io.taig.otter.Constraint
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaDocument
import io.taig.otter.JsonSchemaIssue
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Metadata
import io.taig.otter.codec.JsonSchemaAnnotation
import io.taig.otter.codec.Renderer
import io.taig.otter.http.Parameter

/** The JSON Schema of what a path segment, a query parameter or a header holds.
  *
  * A parameter is written in its own alphabet rather than a JSON one, so it needs its own renderer -- but the document
  * it produces is a JSON Schema, and so the shortfalls it reports are [[JsonSchemaIssue]]s and the keywords it emits
  * come from the same [[ConstraintJsonSchema]] every other renderer uses.
  *
  * A parameter's schema describes the value, not the text. That is OpenAPI's own convention -- `type: integer` on a
  * query parameter never meant the bytes were anything but text -- and it is why a [[Parameter.Coerce]] adds nothing
  * here and reports nothing: the laxer spellings a coercion accepts are the same type as the canonical one, so there is
  * no second form for a document to mention. What is genuinely lost, that `?verbose` alone reads as `true`, is lost to
  * OpenAPI's vocabulary and not to this profile's, so no profile could record it as a choice it made.
  */
final class OpenApiParameterRenderer(
    profile: JsonSchemaProfile,
    namespaces: NonEmptyList[Metadata.Namespace]
) extends Renderer[Parameter.Node, JsonSchemaDocument]:
  override def render[W, R](parameter: Parameter.Node[W, R]): JsonSchemaDocument =
    val (schema, issues) = rendered(parameter)

    JsonSchemaDocument(schema, issues.toList)

  private def rendered(parameter: Parameter.Node[?, ?]): (CirceJson, Chain[JsonSchemaIssue]) = parameter match
    case Parameter.Collection.Schema(node)        => annotated(node.metadata, collection(node.self))
    case Parameter.Coerce.Schema(node)            => annotated(node.metadata, coerce(node.self))
    case Parameter.Constant.Schema(node)          => annotated(node.metadata, constant(node.self))
    case Parameter.Enumeration.Schema(node)       => annotated(node.metadata, enumeration(node.self))
    case Parameter.Primitive.Boolean.Schema(node) => annotated(node.metadata, boolean(node.self))
    case Parameter.Primitive.Number.Schema(node)  => annotated(node.metadata, number(node.self))
    case Parameter.Primitive.Text.Schema(node)    => annotated(node.metadata, text(node.self))

  private def annotated(
      metadata: Metadata,
      rendered: (CirceJson, Chain[JsonSchemaIssue])
  ): (CirceJson, Chain[JsonSchemaIssue]) =
    (JsonSchemaAnnotation(namespaces, metadata, rendered._1), rendered._2)

  private def collection(
      schema: Self.Collection[Parameter.Value.Node, ?, ?]
  ): (CirceJson, Chain[JsonSchemaIssue]) = schema match
    case Self.Collection.Modify(self, _, _)             => collection(self)
    case Self.Collection.Chained(reference, validation) => items(reference.value, validation.constraints)
    case Self.Collection.Indexed(reference, validation) => items(reference.value, validation.constraints)
    case Self.Collection.Linked(reference, validation)  => items(reference.value, validation.constraints)

  private def items(
      element: Parameter.Value.Node[?, ?],
      constraints: Chain[Constraint.Collection]
  ): (CirceJson, Chain[JsonSchemaIssue]) =
    val (rendered, issues) = this.rendered(element)
    val (keywords, dropped) = this.keywords(constraints.widen[Constraint])

    (JsonSchema.merge(JsonSchema.merge(JsonSchema.typed("array"), "items" -> rendered), keywords*), issues ++ dropped)

  /** The canonical form, which is the whole of what a coercion has to say in a position that is text either way. */
  private def coerce(schema: Self.Coerce[Parameter.Primitive.Node, ?, ?]): (CirceJson, Chain[JsonSchemaIssue]) =
    schema match
      case Self.Coerce.Modify(self, _, _) => coerce(self)
      case Self.Coerce.Root(reference)    => rendered(reference.value)

  private def constant(schema: Self.Constant[Parameter.Primitive.Node, ?, ?]): (CirceJson, Chain[JsonSchemaIssue]) =
    schema match
      case Self.Constant.Modify(self, _, _)        => constant(self)
      case Self.Constant.Root(reference, value, _) =>
        val literal = ParameterPrimitiveEncoder.encode(reference.value, value.value)

        (JsonSchema.obj("const" -> CirceJson.fromString(literal)), Chain.empty)

  private def enumeration(
      schema: Self.Enumeration[Parameter.Primitive.Node, ?, ?]
  ): (CirceJson, Chain[JsonSchemaIssue]) = schema match
    case Self.Enumeration.Modify(self, _, _)       => enumeration(self)
    case Self.Enumeration.Root(reference, mapping) =>
      val literals = mapping.values.toList
        .map(mapping.inj)
        .map(ParameterPrimitiveEncoder.encode(reference.value, _))
        .map(CirceJson.fromString)

      (JsonSchema.obj("enum" -> CirceJson.fromValues(literals)), Chain.empty)

  private def boolean(schema: Self.Primitive.Boolean[?, ?]): (CirceJson, Chain[JsonSchemaIssue]) = schema match
    case Self.Primitive.Boolean.Modify(self, _, _) => boolean(self)
    case Self.Primitive.Boolean.Root               => (JsonSchema.typed("boolean"), Chain.empty)

  /** `integer` where the carrier has no fractional part, `number` where it has.
    *
    * The distinction is the carrier's and not the constraint's: an `Int` cannot hold `1.5` whatever a validation says,
    * and a `BigDecimal` can whatever it says.
    */
  private def number(schema: Self.Primitive.Number[?, ?]): (CirceJson, Chain[JsonSchemaIssue]) = schema match
    case Self.Primitive.Number.Modify(self, _, _)     => number(self)
    case Self.Primitive.Number.BigDecimal(validation) => numeric("number", validation.constraints)
    case Self.Primitive.Number.Double(validation)     => numeric("number", validation.constraints)
    case Self.Primitive.Number.Float(validation)      => numeric("number", validation.constraints)
    case Self.Primitive.Number.BigInteger(validation) => numeric("integer", validation.constraints)
    case Self.Primitive.Number.Int(validation)        => numeric("integer", validation.constraints)
    case Self.Primitive.Number.Long(validation)       => numeric("integer", validation.constraints)

  private def numeric(
      name: String,
      constraints: Chain[Constraint.Primitive.Number]
  ): (CirceJson, Chain[JsonSchemaIssue]) =
    val (keywords, dropped) = this.keywords(constraints.widen[Constraint])

    (JsonSchema.merge(JsonSchema.typed(name), keywords*), dropped)

  private def text(schema: Self.Primitive.Text[?, ?]): (CirceJson, Chain[JsonSchemaIssue]) = schema match
    case Self.Primitive.Text.Modify(self, _, _) => text(self)
    case Self.Primitive.Text.Root(validation)   =>
      val (keywords, dropped) = this.keywords(validation.constraints.widen[Constraint])

      (JsonSchema.merge(JsonSchema.typed("string"), keywords*), dropped)
    case Self.Primitive.Text.Format(name, _, _) =>
      profile.format(name) match
        case Some(format) =>
          (JsonSchema.merge(JsonSchema.typed("string"), "format" -> CirceJson.fromString(format)), Chain.empty)
        case None => (JsonSchema.typed("string"), Chain.one(JsonSchemaIssue.Format(None, name)))

  /** What the profile has a keyword for, and a note for every constraint it has none for. */
  private def keywords(constraints: Chain[Constraint]): (List[(String, CirceJson)], Chain[JsonSchemaIssue]) =
    constraints.foldLeft((List.empty[(String, CirceJson)], Chain.empty[JsonSchemaIssue])): (accumulated, constraint) =>
      val (keywords, issues) = accumulated

      profile.keyword(constraint) match
        case Some(keyword) => (keywords :+ keyword, issues)
        case None          => (keywords, issues :+ JsonSchemaIssue.Dropped(None, constraint))

object OpenApiParameterRenderer:
  /** Whether a named member has to be there, which OpenAPI keeps beside a schema rather than inside it.
    *
    * Written for any field and not only a parameter's, because a [[io.taig.otter.http.Part]] is a field too and a part
    * that may be left out is `not required` in exactly the same sense. A field holding a default is not required: the
    * whole point of a default is that the caller may say nothing.
    */
  def required(field: Self.Field[?, ?, ?]): Boolean = field match
    case Self.Field.Root(_, _)         => true
    case Self.Field.Modify(self, _, _) => OpenApiParameterRenderer.required(self)
    case Self.Field.Optional(_)        => false
    case Self.Field.Default(_, _)      => false
