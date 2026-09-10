package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.JsonSchemaIssue

import scala.collection.immutable.ListMap
import scala.collection.immutable.Queue

/** Definitions, allocated names, and the active traversal of one document. */
final case class JsonSchemaContext(
    definitions: ListMap[String, CirceJson],
    stack: Queue[String],
    recursive: Boolean,
    issues: Chain[JsonSchemaIssue],
    names: DefinitionNames = DefinitionNames.Empty
):
  def push(name: String): JsonSchemaContext = copy(stack = stack.enqueue(name))

  def recursive(value: Boolean): JsonSchemaContext = copy(recursive = value)

  def updated(name: String, definition: CirceJson): JsonSchemaContext =
    copy(definitions = definitions.updated(name, definition))

  /** The `$defs` entry being rendered, which is the most a hoisting renderer can say about where an issue was found. */
  def definition: Option[String] = stack.lastOption

  def issue(f: Option[String] => JsonSchemaIssue): JsonSchemaContext =
    copy(issues = issues.append(f(definition)))

  /** Leaves the definitions and issues gathered so far, and forgets that anything was in progress. */
  def restore(context: JsonSchemaContext): JsonSchemaContext =
    copy(stack = context.stack, recursive = context.recursive)

object JsonSchemaContext:
  val Empty: JsonSchemaContext =
    JsonSchemaContext(definitions = ListMap.empty, stack = Queue.empty, recursive = false, issues = Chain.empty)
