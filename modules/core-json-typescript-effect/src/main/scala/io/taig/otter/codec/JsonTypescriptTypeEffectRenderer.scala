package io.taig.otter.codec

import cats.data.NonEmptyList
import io.taig.otter.Coerce
import io.taig.otter.Json
import io.taig.otter.Side
import io.taig.otter.Typescript

/** The types of the expressions emitted by [[JsonTypescriptExpressionEffectRenderer]]. */
final class JsonTypescriptTypeEffectRenderer(
    side: Side,
    projection: JsonTypescriptTarget.Projection,
    renderer: Renderer[Json.Node, Typescript.Type]
) extends Renderer[Json.Node, Typescript.Type]:
  private val wire = new JsonTypescriptTypeRenderer(side, renderer)

  override def render[W, R](json: Json.Node[W, R]): Typescript.Type = json match
    case Json.Coerce.Schema(node) if side == Side.Read                                     => coerce(node.self)
    case Json.Record.Schema(node) if projection == JsonTypescriptTarget.Projection.Decoded =>
      Typescript.Type.Object(node.self.fields.toList.map: reference =>
        val field = reference.value
        Json.presence(side, field.self.metadata, field.self.self) match
          case Json.Presence.OptionalNullable =>
            Typescript.Type.Field(field.self.self.name, renderer.render(field.self.self.schema.value), optional = true)
          case _ => wire.field(field))
    case _ => wire.render(json)

  private def coerce[W, R](schema: Coerce[Json.Primitive.Node, W, R]): Typescript.Type = schema match
    case Coerce.Modify(self, _, _) => coerce(self)
    case Coerce.Root(reference)    =>
      val primitive = reference.value match
        case Json.Primitive.Boolean.Schema(_) => Typescript.Type.Symbol("boolean", Nil)
        case Json.Primitive.Number.Schema(_)  => Typescript.Type.Symbol("number", Nil)
        case Json.Primitive.Text.Schema(_)    => Typescript.Type.Symbol("string", Nil)

      projection match
        case JsonTypescriptTarget.Projection.Decoded => primitive
        case JsonTypescriptTarget.Projection.Encoded =>
          reference.value match
            case Json.Primitive.Boolean.Schema(_) =>
              Typescript.Type.Union(
                NonEmptyList.of(
                  primitive,
                  Typescript.Type.Literal.String("true"),
                  Typescript.Type.Literal.String("false")
                )
              )
            case Json.Primitive.Number.Schema(_) =>
              Typescript.Type.Union(NonEmptyList.of(primitive, Typescript.Type.Symbol("string", Nil)))
            case Json.Primitive.Text.Schema(_) =>
              Typescript.Type.Union(
                NonEmptyList.of(
                  primitive,
                  Typescript.Type.Symbol("number", Nil),
                  Typescript.Type.Symbol("boolean", Nil)
                )
              )
