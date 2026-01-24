package io.taig.otter.codec

import cats.Monoid
import cats.syntax.all.*
import io.taig.otter.Schema
import io.taig.otter.Typescript

import scala.collection.immutable.ListMap
import scala.collection.immutable.Queue

final case class JsonTypescriptEffectContext(
    definitions: ListMap[String, (Typescript.Type, Typescript.Expression)],
    stack: Queue[String],
    recursive: Boolean
):
  def updated(
      name: String,
      tpe: Typescript.Type,
      expression: Typescript.Expression
  ): JsonTypescriptEffectContext = copy(definitions = definitions.updated(name, (tpe, expression)))

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

  def +(name: String): JsonTypescriptEffectContext = copy(stack = stack.enqueue(name))

  def recursive(value: Boolean): JsonTypescriptEffectContext = copy(recursive = value)

  def combine(context: JsonTypescriptEffectContext): JsonTypescriptEffectContext = JsonTypescriptEffectContext(
    definitions = definitions ++ context.definitions,
    stack = stack ++ context.stack,
    recursive = recursive || context.recursive
  )

object JsonTypescriptEffectContext:
  val Empty: JsonTypescriptEffectContext = JsonTypescriptEffectContext(
    definitions = ListMap.empty,
    stack = Queue.empty,
    recursive = false
  )

  given Monoid[JsonTypescriptEffectContext]:
    override def empty: JsonTypescriptEffectContext = Empty

    override def combine(x: JsonTypescriptEffectContext, y: JsonTypescriptEffectContext): JsonTypescriptEffectContext =
      x.combine(y)
