package io.taig.otter.codec

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Metadata
import io.taig.otter.Side
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

/** Hoists schemas by instance identity and allocates distinct names when definitions cannot be shared. */
final class JsonStateTypescriptRenderer(
    side: Side,
    namespaces: NonEmptyList[Metadata.Namespace],
    target: JsonTypescriptTarget,
    rename: String => String,
    expression: Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]] => Renderer[
      Json.Node,
      State[JsonTypescriptContext, Typescript.Expression]
    ]
) extends Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]]:
  private lazy val body: Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]] =
    JsonTypescriptExpressionOverrideRenderer(namespaces, expression(this))

  private def structural(json: Json.Node[?, ?], context: JsonTypescriptContext): Typescript.Type =
    lazy val renderer: Renderer[Json.Node, Typescript.Type] = JsonTypescriptTypeOverrideRenderer(
      namespaces,
      new JsonTypescriptTypeRenderer(
        side,
        Renderer([w, r] =>
          (child: Json.Node[w, r]) =>
            context.names.get(child).flatMap(base => context.bindings.get((base, side))) match
              case Some(name) => Typescript.Type.Symbol(name, parameters = Nil)
              case None       => renderer.render(child)
        )
      )
    )

    renderer.render(json)

  override def render[W, R](json: Json.Node[W, R]): State[JsonTypescriptContext, Typescript.Expression] =
    State: initial =>
      Json.name(namespaces, json) match
        case None       => body.render(json).run(initial).value
        case Some(hint) =>
          val (names, base) = initial.names.assign(json, hint)
          val context = initial.copy(names = names)

          context.bindings.get((base, side)) match
            case Some(name) =>
              val symbol = Typescript.Expression.Symbol(name)
              if context.stack.contains(name) then (context.recursive(true), target.suspend(symbol))
              else if context.definitions.contains(name) then (context, symbol)
              else
                val (update, definition) = definitionOf(json, name, context)
                (update.restore(context).updated(name, definition), symbol)
            case None =>
              val other = side match
                case Side.Read  => Side.Write
                case Side.Write => Side.Read

              context.bindings.get((base, other)) match
                case Some(name) =>
                  val symbol = Typescript.Expression.Symbol(name)
                  val (update, definition) = definitionOf(json, name, context.bind(base, side, name))

                  if context.definitions.get(name).contains(definition) then (update.restore(context), symbol)
                  else declare(json, base, context)
                case None => declare(json, base, context)

  private def declare[W, R](
      json: Json.Node[W, R],
      base: String,
      context: JsonTypescriptContext
  ): (JsonTypescriptContext, Typescript.Expression) =
    val name = context.available(rename(base))
    val (update, definition) = definitionOf(json, name, context.bind(base, side, name))

    (update.restore(context).updated(name, definition), Typescript.Expression.Symbol(name))

  private def definitionOf[W, R](
      json: Json.Node[W, R],
      name: String,
      context: JsonTypescriptContext
  ): (JsonTypescriptContext, JsonTypescriptDefinition) =
    val symbol = Typescript.Expression.Symbol(name)
    val (update, expression) = body.render(json).run(context.push(name)).value
    val declared = Json
      .attr(namespaces, Json.metadata(json), TypescriptKeys.tpe)
      .orElse(Option.when(update.recursive)(structural(json, update)))

    val definition = declared.fold(JsonTypescriptDefinition(target.inferred(symbol), none, expression)): tpe =>
      JsonTypescriptDefinition(tpe, target.annotation(name).some, expression)

    (update, definition)
