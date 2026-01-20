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

/*
type ContextState[A, B] = State[ContextState.Context[A], B]

object ContextState:
  final case class Context[+A](references: ListMap[String, A], stack: SortedSet[String], recursion: SortedSet[String]):
    def map[B](f: A => B): ContextState.Context[B] = copy(references = references.view.mapValues(f).to(ListMap))

    def modifyReferences[A1 >: A](f: ListMap[String, A] => ListMap[String, A1]): ContextState.Context[A1] =
      copy(references = f(references))

    def modfyStack(f: SortedSet[String] => SortedSet[String]): ContextState.Context[A] = copy(stack = f(stack))

    def push(stack: String): ContextState.Context[A] = modfyStack(_ + stack)

    def pop(stack: String): ContextState.Context[A] = modfyStack(_ - stack).modifyRecursion(_ - stack)

    def modifyRecursion(f: SortedSet[String] => SortedSet[String]): ContextState.Context[A] =
      copy(recursion = f(recursion))

    def recurse(name: String): ContextState.Context[A] = modifyRecursion(_ + name)
 */

object JsonZodTypescriptExpressionsRenderer
    extends Renderer[Json.Read, State[JsonZodTypescriptExpressionsRenderer.Context, Typescript.Expression]]:
  final case class Context(definitions: ListMap[String, Typescript.Expression], stack: Queue[String]):
    def updated(name: String, expression: Typescript.Expression): JsonZodTypescriptExpressionsRenderer.Context =
      copy(definitions = definitions.updated(name, expression))

    def +(name: String): JsonZodTypescriptExpressionsRenderer.Context = copy(stack = stack.enqueue(name))

    def declarations: List[Typescript.Statement.Declaration] = definitions.toList
      .map(Typescript.Statement.Declaration.Constant.apply)

    def combine(context: JsonZodTypescriptExpressionsRenderer.Context): JsonZodTypescriptExpressionsRenderer.Context =
      Context(definitions = definitions ++ context.definitions, stack = stack ++ context.stack)

  object Context:
    val Empty: JsonZodTypescriptExpressionsRenderer.Context = Context(definitions = ListMap.empty, stack = Queue.empty)

    given Monoid[JsonZodTypescriptExpressionsRenderer.Context]:
      override def empty: Context = Empty

      override def combine(x: Context, y: Context): Context = x.combine(y)

  val renderer = JsonZodTypescriptExpressionRenderer(renderer = this)

  override def render[A](json: Json.Read[A]): State[Context, Typescript.Expression] = State: context =>
    json.attr(JsonZod.Namespace, Zod.Namespace, Metadata.Namespace.Global)(key = Keys.name) match
      case Some(name) =>
        if context.stack.contains(name)
        then
          (
            context,
            z(property =
              Typescript.Expression.Call(
                name = "lazy",
                arguments = List(Typescript.Expression.Arrow(body = Typescript.Expression.Symbol(name)))
              )
            )
          )
        else
          context.definitions.get(name) match
            case Some(_) => (context, Typescript.Expression.Symbol(name))
            case None    =>
              val (update, expression) = renderer.render(json).run(context + name).value
              (
                (context |+| update).updated(name, expression),
                Typescript.Expression.Symbol(name)
              )
      case None =>
        val (update, expression) = renderer.render(json).run(context).value
        (context |+| update, expression)
