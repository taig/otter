package io.taig.otter.codec

import cats.Id
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.JsonTypescriptEffect
import io.taig.otter.Keys
import io.taig.otter.Metadata
import io.taig.otter.Schema
import io.taig.otter.Typescript
import io.taig.otter.TypescriptEffect

object JsonStateTypescriptExpressionEffectRenderer
    extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], State[JsonTypescriptEffectContext, Typescript.Expression]]:
  val name: (Json.Read[?] | Json.Write[?]) => Option[String] = json =>
    val metadata = json match
      case json: Json.Read[?]  => json.metadata
      case json: Json.Write[?] => json.metadata

    NonEmptyList
      .of(JsonTypescriptEffect.Namespace, TypescriptEffect.Namespace, Metadata.Namespace.Global)
      .foldl(none[String]):
        case (None, namespace)     => metadata.get(namespace, Keys.name)
        case (result @ Some(_), _) => result

  object renderer:
    val expression: Renderer[
      [a] =>> Json.Read[a] | Json.Write[a],
      State[JsonTypescriptEffectContext, Typescript.Expression]
    ] = JsonTypescriptExpressionOverrideRenderer(
      namespaces = NonEmptyList.of(
        JsonTypescriptEffect.Namespace,
        TypescriptEffect.Namespace,
        Metadata.Namespace.Global
      ),
      renderer = JsonTypescriptExpressionEffectRenderer(
        renderer = JsonStateTypescriptExpressionEffectRenderer
      )
    )

    val typescriptType: Renderer[[a] =>> Json.Read[a] | Json.Write[a], Typescript.Type] =
      JsonTypescriptTypeRenderer[Id](
        renderer = Renderer([A] =>
          (json: (Json.Read[A] | Json.Write[A])) =>
            name(json) match
              case Some(name) => Typescript.Type.Symbol(name, parameters = Nil)
              case None       => typescriptType.render(json)
        )
      )

    def effectType(symbol: Typescript.Expression) = JsonTypescriptTypeOverrideRenderer[Id](
      namespaces = NonEmptyList.of(
        JsonTypescriptEffect.Namespace,
        TypescriptEffect.Namespace,
        Metadata.Namespace.Global
      ),
      renderer = Renderer.pure(Schema.tpe(Typescript.Type.TypeOf(symbol)))
    )

  override def render[A](
      json: Json.Read[A] | Json.Write[A]
  ): State[JsonTypescriptEffectContext, Typescript.Expression] = State: context =>
    name(json) match
      case Some(name) =>
        val symbol = Typescript.Expression.Symbol(name)

        if context.stack.contains(name)
        then (context.recursive(true), Schema.suspend(symbol))
        else
          context.definitions.get(name) match
            case Some(_) => (context, symbol)
            case None    =>
              val (update, expression) = renderer.expression.render(json).run(context + name).value
              val tpe =
                if update.recursive
                then renderer.typescriptType.render(json)
                else renderer.effectType(symbol).render(json)

              ((context |+| update).recursive(false).updated(name, tpe, expression), symbol)
      case None => renderer.expression.render(json).run(context).value.leftMap(context |+| _)
