package io.taig.otter.codec

import io.taig.otter.Typescript

import scala.collection.immutable.ListMap
import scala.collection.immutable.Queue

/** What a renderer carries along while it walks a schema.
  *
  * `definitions` are the declarations hoisted so far, in the order they were finished, so that a name is always
  * declared before it is used. `stack` is the names whose bodies are still being rendered, which is how a cycle is
  * noticed at all: reaching a name that is already on it means the schema refers to itself. `recursive` records that
  * such a reference was made, and is read by whichever definition was being rendered when it happened -- that one and
  * no other, which is why opening a definition clears it and finishing one puts back what the definition around it had.
  */
final case class JsonTypescriptContext(
    definitions: ListMap[String, JsonTypescriptDefinition],
    stack: Queue[String],
    recursive: Boolean
):
  /** Opens a definition, which has made no recursive reference yet however many the one around it has made. Carrying
    * the flag in would have every name that merely follows a cycle in the same body declare its type by hand.
    */
  def push(name: String): JsonTypescriptContext = copy(stack = stack.enqueue(name), recursive = false)

  def recursive(value: Boolean): JsonTypescriptContext = copy(recursive = value)

  def updated(name: String, definition: JsonTypescriptDefinition): JsonTypescriptContext =
    copy(definitions = definitions.updated(name, definition))

  /** Leaves the definitions gathered so far, and forgets that anything was in progress. */
  def restore(context: JsonTypescriptContext): JsonTypescriptContext =
    copy(stack = context.stack, recursive = context.recursive)

  def declarations: List[Typescript.Statement.Declaration] =
    definitions.toList.flatMap((name, definition) => definition.declarations(name))

object JsonTypescriptContext:
  val Empty: JsonTypescriptContext =
    JsonTypescriptContext(definitions = ListMap.empty, stack = Queue.empty, recursive = false)
