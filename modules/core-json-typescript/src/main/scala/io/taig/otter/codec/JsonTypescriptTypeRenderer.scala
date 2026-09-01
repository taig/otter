package io.taig.otter.codec

import cats.data.NonEmptyList
import io.taig.otter.Coerce
import io.taig.otter.Json
import io.taig.otter.JsonTypescript
import io.taig.otter.Optional
import io.taig.otter.Side
import io.taig.otter.Tuple
import io.taig.otter.Typescript

/** Describes one side of a schema as a plain TypeScript type.
  *
  * A generator that emits schema values gets its types for free by inference, so this is not the main output. It is
  * what a definition that refers to itself needs: TypeScript cannot infer a type through a cycle, so a recursive
  * declaration has to be told its own shape, and only a structural type can tell it.
  *
  * `renderer` is where a child goes. Handing it in rather than recursing directly is what lets the caller substitute a
  * name for a child that has one, which is the whole reason this terminates on `json.tree`.
  */
final class JsonTypescriptTypeRenderer(side: Side, renderer: Renderer[Json.Node, Typescript.Type])
    extends Renderer[Json.Node, Typescript.Type]:
  override def render[W, R](json: Json.Node[W, R]): Typescript.Type = json match
    case Json.Coerce.Schema(node)     => coerce(node.self)
    case Json.Collection.Schema(node) =>
      Typescript.Type.Symbol("ReadonlyArray", List(child(node.self.schema.value)))
    case Json.Constant.Schema(node)   => JsonTypescriptTypeRenderer.literal(JsonTypescriptLiteral.constant(node.self))
    case Json.Dictionary.Schema(node) =>
      Typescript.Type.Symbol("Record", List(JsonTypescriptTypeRenderer.Text, child(node.self.schema.value)))
    case Json.Enumeration.Schema(node) =>
      Typescript.Type.Union(JsonTypescriptLiteral.enumeration(node.self).map(JsonTypescriptTypeRenderer.literal))
    case Json.Optional.Schema(node)       => optional(node.self)
    case Json.Primitive.Boolean.Schema(_) => JsonTypescriptTypeRenderer.Boolean
    case Json.Primitive.Number.Schema(_)  => JsonTypescriptTypeRenderer.Number
    case Json.Primitive.Text.Schema(_)    => JsonTypescriptTypeRenderer.Text
    case Json.Record.Schema(node)         =>
      Typescript.Type.Object(node.self.fields.toList.map(reference => field(reference.value)))
    case Json.Tuple.Schema(node) => tuple(node.self)
    case Json.Union.Schema(node) =>
      Typescript.Type.Union(
        node.self.branches.toNonEmptyList.map(reference => child(reference.value.self.self.schema.value))
      )

  private def child(json: Json.Node[?, ?]): Typescript.Type = renderer.render(json)

  /** A record's member, whose key may be absent and whose value may be empty, depending on the side. */
  def field(json: Json.Field.Node[?, ?]): Typescript.Type.Field =
    val tpe = child(json.self.self.schema.value)

    JsonTypescript.presence(side, json.self.metadata, json.self.self) match
      case JsonTypescript.Presence.Required => Typescript.Type.Field(json.self.self.name, tpe, optional = false)
      case JsonTypescript.Presence.Nullable =>
        Typescript.Type.Field(json.self.self.name, nullable(tpe), optional = false)
      case JsonTypescript.Presence.Optional         => Typescript.Type.Field(json.self.self.name, tpe, optional = true)
      case JsonTypescript.Presence.OptionalNullable =>
        Typescript.Type.Field(json.self.self.name, nullable(tpe), optional = true)

  /** The laxer wire forms the decoder normalises before handing over, which only the read side sees. */
  private def coerce[W, R](schema: Coerce[Json.Primitive.Node, W, R]): Typescript.Type = schema match
    case Coerce.Modify(self, _, _) => coerce(self)
    case Coerce.Root(reference)    =>
      val primitive = child(reference.value)

      side match
        case Side.Write => primitive
        case Side.Read  =>
          reference.value match
            case Json.Primitive.Boolean.Schema(_) =>
              Typescript.Type.Union(NonEmptyList.of(primitive, JsonTypescriptTypeRenderer.Text))
            case Json.Primitive.Number.Schema(_) =>
              Typescript.Type.Union(NonEmptyList.of(primitive, JsonTypescriptTypeRenderer.Text))
            case Json.Primitive.Text.Schema(_) =>
              Typescript.Type.Union(
                NonEmptyList.of(primitive, JsonTypescriptTypeRenderer.Number, JsonTypescriptTypeRenderer.Boolean)
              )

  private def optional[W, R](schema: Optional[Json.Node, W, R]): Typescript.Type = schema match
    case Optional.Modify(self, _, _)    => optional(self)
    case Optional.Root(reference)       => nullable(child(reference.value))
    case Optional.Default(reference, _) =>
      side match
        case Side.Write => child(reference.value)
        case Side.Read  => nullable(child(reference.value))

  private def tuple[W, R](schema: Tuple[Json.Node, W, R]): Typescript.Type =
    val elements = schema.schemas.toList.map(reference => child(reference.value))
    val self = Typescript.Type.Tuple(elements)

    if JsonTypescript.absent(side, schema)
    then Typescript.Type.Union(NonEmptyList.of(self, Typescript.Type.Tuple(elements.map(_ => Typescript.Type.Null))))
    else self

  private def nullable(tpe: Typescript.Type): Typescript.Type = tpe match
    case Typescript.Type.Union(types) => Typescript.Type.Union(types :+ Typescript.Type.Null)
    case tpe                          => Typescript.Type.Union(NonEmptyList.of(tpe, Typescript.Type.Null))

object JsonTypescriptTypeRenderer:
  /** The renderer that spells every child out, however deep.
    *
    * It does not terminate on a schema that refers to itself, and cannot: substituting a name for a child is the only
    * way to stop, and only [[JsonStateTypescriptRenderer]] knows the names.
    */
  def apply(side: Side): JsonTypescriptTypeRenderer =
    lazy val self: JsonTypescriptTypeRenderer =
      new JsonTypescriptTypeRenderer(side, Renderer([w, r] => (json: Json.Node[w, r]) => self.render(json)))

    self

  private val Boolean: Typescript.Type = Typescript.Type.Symbol("boolean", parameters = Nil)
  private val Number: Typescript.Type = Typescript.Type.Symbol("number", parameters = Nil)
  private val Text: Typescript.Type = Typescript.Type.Symbol("string", parameters = Nil)

  private val literal: Typescript.Expression.Literal => Typescript.Type.Literal =
    case Typescript.Expression.Literal.Boolean(value) => Typescript.Type.Literal.Boolean(value)
    case Typescript.Expression.Literal.Number(value)  => Typescript.Type.Literal.Number(value)
    case Typescript.Expression.Literal.String(value)  => Typescript.Type.Literal.String(value)
