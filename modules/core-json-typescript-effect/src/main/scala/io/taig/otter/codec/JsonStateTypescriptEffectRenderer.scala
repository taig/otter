package io.taig.otter.codec

import cats.data.State
import io.taig.otter.Json
import io.taig.otter.Keys

import scala.collection.immutable.ListMap
import io.taig.otter.Typescript
import io.taig.otter.Metadata
import io.taig.otter.codec.JsonStateTypescriptExpressionEffectRenderer.Context
import scala.collection.immutable.Queue
import cats.Monoid
import cats.syntax.all.*
import io.taig.otter.Json.Write
import cats.Id
import io.taig.otter.Schema
import io.taig.otter.JsonTypescriptEffect
import io.taig.otter.TypescriptEffect

object JsonStateTypescriptExpressionEffectRenderer extends Renderer[Json.Write, State[Context, Typescript.Expression]]:
  // TODO is a recursion flag sufficient? Will this break nested recursions?
  final case class Context(
      definitions: ListMap[String, (Typescript.Type, Typescript.Expression)],
      stack: Queue[String],
      recursion: Boolean
  ):
    def updated(
        name: String,
        tpe: Typescript.Type,
        expression: Typescript.Expression
    ): Context = copy(definitions = definitions.updated(name, (tpe, expression)))

    def declarations: List[Typescript.Statement.Declaration] = definitions.toList.flatMap:
      case (name, (tpe, expression)) =>
        val annotation = tpe match
          case Typescript.Type.Member("Schema", Typescript.Type.Member("Schema", Typescript.Type.Symbol("Type", _))) =>
            none
          case _ =>
            Schema(
              Typescript.Type.Symbol(
                name = "ZodType",
                parameters = List(Typescript.Type.Symbol(name, parameters = Nil))
              )
            ).some

        List(
          Typescript.Statement.Declaration.Type(name, tpe),
          Typescript.Statement.Declaration.Constant(name, tpe = annotation, expression)
        )

    def +(name: String): Context = copy(stack = stack.enqueue(name))

    def cyclic: Context = copy(recursion = true)
    def acyclic: Context = copy(recursion = false)

    def combine(context: Context): Context =
      Context(
        definitions = definitions ++ context.definitions,
        stack = stack ++ context.stack,
        recursion = recursion || context.recursion
      )

  object Context:
    val Empty: Context =
      Context(definitions = ListMap.empty, stack = Queue.empty, recursion = false)

    given Monoid[Context]:
      override def empty: Context = Empty

      override def combine(x: Context, y: Context): Context = x.combine(y)

  val name: Json.Write[?] => Option[String] = _.attr(
    JsonTypescriptEffect.Namespace,
    TypescriptEffect.Namespace,
    Metadata.Namespace.Global
  )(key = Keys.name)

  object renderer:
    val expression: Renderer[Json.Write, State[Context, Typescript.Expression]] =
      JsonTypescriptExpressionEffectRenderer(renderer = JsonStateTypescriptExpressionEffectRenderer)

    val tpe: Renderer[Json.Write, Typescript.Type] = JsonTypescriptTypeRenderer[Id](renderer =
      Renderer([A] =>
        (json: Json.Write[A]) =>
          name(json) match
            case Some(name) => Typescript.Type.Symbol(name, parameters = Nil)
            case None       => tpe.render(json)
      )
    )

  override def render[A](json: Json.Write[A]): State[Context, Typescript.Expression] = State: context =>
    name(json) match
      case Some(name) =>
        val symbol = Typescript.Expression.Symbol(name)

        if context.stack.contains(name)
        then
          (
            context.cyclic,
            Schema(
              Typescript.Expression.Call(
                name = "lazy",
                arguments = List(Typescript.Expression.Arrow(body = symbol))
              )
            )
          )
        else
          context.definitions.get(name) match
            case Some(_) => (context, symbol)
            case None    =>
              val (update, expression) = renderer.expression.render(json).run(context + name).value

              val tpe =
                if update.recursion
                then renderer.tpe.render(json)
                else Schema(Schema(Typescript.Type.Symbol("Type", List(Typescript.Type.TypeOf(expression = symbol)))))

              ((context |+| update).acyclic.updated(name, tpe, expression), symbol)
      case None => renderer.expression.render(json).run(context).value.leftMap(context |+| _)
