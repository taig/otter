package io.taig.otter.codec

import cats.data.State
import io.taig.otter.Json
import io.taig.otter.JsonZod
import io.taig.otter.Keys
import io.taig.otter.Zod

import scala.collection.immutable.ListMap
import io.taig.otter.Typescript
import io.taig.otter.Metadata
import scala.collection.immutable.Queue
import cats.Monoid
import io.taig.otter.z
import cats.syntax.all.*

object JsonZodStateTypescriptRenderer
    extends Renderer[Json.Read, State[JsonZodStateTypescriptRenderer.Context, Typescript.Expression]]:
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
    ): JsonZodStateTypescriptRenderer.Context =
      copy(definitions = definitions.updated(name, (tpe, expression)))

    def declarations: List[Typescript.Statement.Declaration] = definitions.toList.flatMap:
      case (name, (tpe, expression)) =>
        val annotation = tpe match
          case Typescript.Type.Member("z", Typescript.Type.Symbol("infer", _)) => none
          case _                                                               =>
            z(property =
              Typescript.Type.Symbol(
                name = "ZodType",
                parameters = List(Typescript.Type.Symbol(name, parameters = Nil))
              )
            ).some

        List(
          Typescript.Statement.Declaration.Type(name, tpe),
          Typescript.Statement.Declaration.Constant(name, tpe = annotation, expression)
        )

    def +(name: String): JsonZodStateTypescriptRenderer.Context = copy(stack = stack.enqueue(name))

    def cyclic: JsonZodStateTypescriptRenderer.Context = copy(recursion = true)
    def acyclic: JsonZodStateTypescriptRenderer.Context = copy(recursion = false)

    def combine(context: JsonZodStateTypescriptRenderer.Context): JsonZodStateTypescriptRenderer.Context =
      Context(
        definitions = definitions ++ context.definitions,
        stack = stack ++ context.stack,
        recursion = recursion || context.recursion
      )

  object Context:
    val Empty: JsonZodStateTypescriptRenderer.Context =
      Context(definitions = ListMap.empty, stack = Queue.empty, recursion = false)

    given Monoid[JsonZodStateTypescriptRenderer.Context]:
      override def empty: Context = Empty

      override def combine(x: Context, y: Context): Context = x.combine(y)

  val renderer = JsonZodFTypescriptExpressionRenderer(renderer = this)

  override def render[A](json: Json.Read[A]): State[Context, Typescript.Expression] = State: context =>
    json.attr(JsonZod.Namespace, Zod.Namespace, Metadata.Namespace.Global)(key = Keys.name) match
      case Some(name) =>
        val symbol = Typescript.Expression.Symbol(name)

        if context.stack.contains(name)
        then
          (
            context.cyclic,
            z(property =
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
              val (update, expression) = renderer.render(json).run(context + name).value

              val tpe =
                if update.recursion
                then Typescript.Type.Object(fields = Nil) // TODO typescript renderer
                else z(property = Typescript.Type.Symbol("infer", List(Typescript.Type.TypeOf(expression = symbol))))

              ((context |+| update).acyclic.updated(name, tpe, expression), symbol)
      case None => renderer.render(json).run(context).value.leftMap(context |+| _)
