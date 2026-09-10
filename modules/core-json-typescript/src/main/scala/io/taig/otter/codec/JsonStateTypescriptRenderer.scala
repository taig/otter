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

  private def structural(
      json: Json.Node[?, ?],
      name: String,
      context: JsonTypescriptContext,
      projection: JsonTypescriptTarget.Projection,
      selfEncoded: Option[String] = None
  ): Typescript.Type =
    val key = projection match
      case JsonTypescriptTarget.Projection.Decoded => TypescriptKeys.tpe
      case JsonTypescriptTarget.Projection.Encoded => TypescriptKeys.encodedType

    lazy val renderer: Renderer[Json.Node, Typescript.Type] = JsonTypescriptTypeOverrideRenderer(
      namespaces,
      target.structural(
        side,
        projection,
        Renderer([w, r] =>
          (child: Json.Node[w, r]) =>
            context.names.get(child).flatMap(base => context.bindings.get((base, side))) match
              case Some(childName) =>
                projection match
                  case JsonTypescriptTarget.Projection.Decoded => Typescript.Type.Symbol(childName, Nil)
                  case JsonTypescriptTarget.Projection.Encoded =>
                    if childName == name then Typescript.Type.Symbol(selfEncoded.getOrElse(name), Nil)
                    else
                      context.definitions.get(childName) match
                        case Some(definition) if definition.annotation.isDefined =>
                          Typescript.Type.Symbol(definition.encoded.fold(childName)(_._1), Nil)
                        case _ => target.encoded(Typescript.Expression.Symbol(childName))
              case None => renderer.render(child)
        )
      ),
      key
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
    val explicit = Json.attr(namespaces, Json.metadata(json), TypescriptKeys.tpe).isDefined ||
      Json.attr(namespaces, Json.metadata(json), TypescriptKeys.encodedType).isDefined

    if !update.recursive && !explicit then (update, JsonTypescriptDefinition(target.inferred(symbol), none, expression))
    else
      val decoded = structural(json, name, update, JsonTypescriptTarget.Projection.Decoded)
      val encoded = structural(json, name, update, JsonTypescriptTarget.Projection.Encoded)
      val tpe = Typescript.Type.Symbol(name, Nil)

      if decoded == encoded then
        (update, JsonTypescriptDefinition(decoded, target.annotation(tpe, tpe).some, expression))
      else
        val encodedName = update.encodedNames.getOrElse(name, update.available(name + "Encoded"))
        val context = update.copy(encodedNames = update.encodedNames.updated(name, encodedName))
        val encodedType = structural(json, name, context, JsonTypescriptTarget.Projection.Encoded, encodedName.some)
        val annotation = target.annotation(tpe, Typescript.Type.Symbol(encodedName, Nil))

        (context, JsonTypescriptDefinition(decoded, annotation.some, expression, (encodedName -> encodedType).some))
