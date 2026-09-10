package io.taig.otter.codec

import io.taig.otter.Side
import io.taig.otter.Typescript
import io.taig.otter.TypescriptIdentifier

import scala.collection.immutable.ListMap
import scala.collection.immutable.Queue

/** Allocated schema names, directional bindings, and declarations collected during a module's traversal. */
final case class JsonTypescriptContext(
    definitions: ListMap[String, JsonTypescriptDefinition],
    stack: Queue[String],
    recursive: Boolean,
    names: DefinitionNames,
    bindings: Map[(String, Side), String],
    encodedNames: Map[String, String] = Map.empty,
    reserved: Set[String] = Set.empty
):
  def push(name: String): JsonTypescriptContext = copy(stack = stack.enqueue(name), recursive = false)

  def recursive(value: Boolean): JsonTypescriptContext = copy(recursive = value)

  def bind(base: String, side: Side, name: String): JsonTypescriptContext =
    copy(bindings = bindings.updated((base, side), name))

  def available(hint: String): String =
    DefinitionNames.available(
      TypescriptIdentifier(hint),
      TypescriptIdentifier.Reserved ++ reserved ++ definitions.keySet ++ bindings.values ++ encodedNames.values ++ stack
    )

  def allocate(hint: String): (JsonTypescriptContext, String) =
    val name = available(hint)
    (copy(reserved = reserved + name), name)

  def updated(name: String, definition: JsonTypescriptDefinition): JsonTypescriptContext =
    copy(definitions = definitions.updated(name, definition))

  def restore(context: JsonTypescriptContext): JsonTypescriptContext =
    copy(stack = context.stack, recursive = context.recursive)

  def declarations: List[Typescript.Statement.Declaration] =
    definitions.toList.flatMap((name, definition) => definition.declarations(name))

object JsonTypescriptContext:
  val Empty: JsonTypescriptContext = JsonTypescriptContext(
    definitions = ListMap.empty,
    stack = Queue.empty,
    recursive = false,
    names = DefinitionNames.Empty,
    bindings = Map.empty
  )
