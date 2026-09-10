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

/** Hoists named schemas and detects recursion using names allocated to schema instances. */
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
    name(json) match
      case None       => self.render(json)
      case Some(hint) =>
        State
          .get[JsonSchemaContext]
          .flatMap: context =>
            val (names, name) = context.names.assign(json, hint)

            State
              .modify[JsonSchemaContext](_.copy(names = names))
              .flatMap: _ =>
                profile.definitions match
                  case None =>
                    if context.stack.contains(name) then
                      State
                        .modify[JsonSchemaContext](_.issue(JsonSchemaIssue.Recursive(_, name)))
                        .as(JsonSchema.Anything)
                    else
                      State
                        .modify[JsonSchemaContext](_.push(name))
                        .flatMap(_ => self.render(json))
                        .flatMap(schema => State.modify[JsonSchemaContext](_.restore(context)).as(schema))
                  case Some(definitions) =>
                    val reference = JsonSchema.ref(definitions, name)

                    if context.stack.contains(name) then
                      val recursive =
                        if profile.recursion then State.modify[JsonSchemaContext](_.recursive(true))
                        else
                          State.modify[JsonSchemaContext](_.recursive(true).issue(JsonSchemaIssue.Recursive(_, name)))

                      recursive.as(reference)
                    else if context.definitions.contains(name) then reference.pure
                    else
                      for
                        _ <- State.modify[JsonSchemaContext](_.push(name))
                        schema <- self.render(json)
                        _ <- State.modify[JsonSchemaContext](update => update.restore(context).updated(name, schema))
                      yield reference
