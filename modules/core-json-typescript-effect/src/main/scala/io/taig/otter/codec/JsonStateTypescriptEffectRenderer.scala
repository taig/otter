package io.taig.otter.codec

import cats.data.State
import io.taig.otter.Json

import scala.collection.immutable.ListMap
import io.taig.otter.Typescript
import io.taig.otter.codec.JsonStateTypescriptExpressionEffectRenderer.Context
import scala.collection.immutable.Queue
import cats.Monoid
import cats.syntax.all.*
import cats.Id
import io.taig.otter.Schema
import cats.data.NonEmptyList
import io.taig.otter.JsonTypescriptEffect
import io.taig.otter.TypescriptEffect
import io.taig.otter.Metadata
import io.taig.otter.Keys

object JsonStateTypescriptExpressionEffectRenderer
    extends Renderer[[a] =>> Json.Read[a] | Json.Write[a], State[Context, Typescript.Expression]]:
  // TODO is a recursion flag sufficient? Will this break nested recursions?
  final case class Context(
      definitions: ListMap[String, (Typescript.Type, Typescript.Expression)],
      stack: Queue[String],
      recursive: Boolean
  ):
    def updated(
        name: String,
        tpe: Typescript.Type,
        expression: Typescript.Expression
    ): Context = copy(definitions = definitions.updated(name, (tpe, expression)))

    def declarations: List[Typescript.Statement.Declaration] = definitions.toList.flatMap:
      case (name, (tpe, expression)) =>
        val annotation = tpe match
          case Schema(Schema(Typescript.Type.Symbol("Type", _))) => none
          case _                                                 =>
            Schema(
              Typescript.Type.Symbol(
                name = "Schema",
                parameters = List(Typescript.Type.Symbol(name, parameters = Nil))
              )
            ).some

        List(
          Typescript.Statement.Declaration.Type(exported = true, name, tpe),
          Typescript.Statement.Declaration.Constant(exported = true, name, tpe = annotation, expression)
        )

    def +(name: String): Context = copy(stack = stack.enqueue(name))

    def recursive(value: Boolean): Context = copy(recursive = value)

    def combine(context: Context): Context = Context(
      definitions = definitions ++ context.definitions,
      stack = stack ++ context.stack,
      recursive = recursive || context.recursive
    )

  object Context:
    val Empty: Context =
      Context(definitions = ListMap.empty, stack = Queue.empty, recursive = false)

    given Monoid[Context]:
      override def empty: Context = Empty

      override def combine(x: Context, y: Context): Context = x.combine(y)

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
    val expression: Renderer[[a] =>> Json.Read[a] | Json.Write[a], State[Context, Typescript.Expression]] =
      JsonTypescriptExpressionOverrideRenderer(
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

  override def render[A](json: Json.Read[A] | Json.Write[A]): State[Context, Typescript.Expression] = State: context =>
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
