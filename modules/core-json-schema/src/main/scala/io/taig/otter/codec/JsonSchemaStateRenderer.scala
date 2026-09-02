package io.taig.otter.codec

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaIssue
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Metadata

/** Gives a named schema a definition of its own, and breaks the cycle when one refers to itself.
  *
  * The fixpoint, and the simpler half of the one `JsonStateTypescriptRenderer` is: a `$ref` is a JSON Pointer, so
  * nothing has to be suspended and no definition has to be told its type. What is left is only the naming:
  *
  *   - a schema with no name is rendered where it stands, and whatever its children hoisted comes along;
  *   - a name already being rendered is a cycle, so the reference points back at it;
  *   - a name already defined is just a reference;
  *   - otherwise the body is rendered with the name in progress and registered under it.
  *
  * `body` is handed `this` rather than itself, so that a child reaching a named schema comes back through the naming
  * rules instead of inlining it. That knot is why the recursion terminates.
  *
  * A name is the only thing that can stop a cycle, so a recursive schema has to carry [[io.taig.otter.Keys.name]] on
  * the value that is reached again -- the `lazy val` itself, not a wrapper around it. Naming the wrapper leaves the
  * inner reference anonymous, and a renderer that never meets a name it has already met does not terminate. Note also
  * that `zip` and `alt` rebuild a node with empty metadata, so a name does not survive `:*` or `:+`: name the schema
  * the composition produces, not one of its operands.
  */
final class JsonSchemaStateRenderer(
    namespaces: NonEmptyList[Metadata.Namespace],
    profile: JsonSchemaProfile,
    body: Renderer[Json.Node, State[JsonSchemaContext, CirceJson]] => Renderer[
      Json.Node,
      State[JsonSchemaContext, CirceJson]
    ]
) extends Renderer[Json.Node, State[JsonSchemaContext, CirceJson]]:
  private lazy val self: Renderer[Json.Node, State[JsonSchemaContext, CirceJson]] =
    JsonSchemaOverrideRenderer(namespaces, body(this))

  private def name(json: Json.Node[?, ?]): Option[String] = Json.name(namespaces, json)

  override def render[W, R](json: Json.Node[W, R]): State[JsonSchemaContext, CirceJson] =
    (name(json), profile.definitions) match
      case (None, _) => self.render(json)
      /* A consumer that will not follow a reference gets everything in place, which is only impossible if the schema
       * refers to itself -- and there the alternative to a document that says nothing is one that does not exist. */
      case (Some(name), None) =>
        State.get[JsonSchemaContext].flatMap { context =>
          if context.stack.contains(name) then
            State
              .modify[JsonSchemaContext](_.issue(JsonSchemaIssue.Recursive(_, name)))
              .as(JsonSchema.Anything)
          else
            State
              .modify[JsonSchemaContext](_.push(name))
              .flatMap(_ => self.render(json))
              .flatMap(schema => State.modify[JsonSchemaContext](_.restore(context)).as(schema))
        }
      case (Some(name), Some(definitions)) =>
        val reference = JsonSchema.ref(definitions, name)

        State.get[JsonSchemaContext].flatMap { context =>
          if context.stack.contains(name) then
            val recursive =
              if profile.recursion then State.modify[JsonSchemaContext](_.recursive(true))
              else State.modify[JsonSchemaContext](_.recursive(true).issue(JsonSchemaIssue.Recursive(_, name)))

            recursive.as(reference)
          else if context.definitions.contains(name) then reference.pure
          else
            for
              _ <- State.modify[JsonSchemaContext](_.push(name))
              schema <- self.render(json)
              _ <- State.modify[JsonSchemaContext](update => update.restore(context).updated(name, schema))
            yield reference
        }
