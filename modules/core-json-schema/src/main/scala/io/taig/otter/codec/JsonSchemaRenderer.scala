package io.taig.otter.codec

import cats.data.NonEmptyList
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaDocument
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Metadata
import io.taig.otter.Side

import scala.collection.immutable.ListMap

/** Turns a JSON schema into a JSON Schema document.
  *
  * [[reader]] and [[writer]] each describe one side. Which one you want follows from who is producing the document the
  * schema describes: a model asked for structured output is producing what *you* read, so the schema you hand it is the
  * read side, and the write side is what you would publish for someone reading what you send. They are not the same
  * document whenever a field is optional or holds a default, which is the whole reason a [[Side]] has to be given -- a
  * codec never has to ask, because the side it works on is the side it was called from.
  *
  * That pairing is the point of the module: the same schema value produces the document a producer is held to and the
  * decoder its answer is read by, so the two cannot drift.
  *
  * A profile that cannot say something says nothing and records it, so a document always comes back and
  * [[JsonSchemaDocument.toEither]] is how a caller insists. Nothing here validates a document against an instance; what
  * a schema constrains is enforced when it is decoded.
  */
object JsonSchemaRenderer:
  /** What a document read under this schema may look like. The side to hand a producer. */
  def reader(profile: JsonSchemaProfile): Renderer[Json.Node, JsonSchemaDocument] =
    JsonSchemaRenderer(Side.Read, profile)

  /** What a document written under this schema looks like. */
  def writer(profile: JsonSchemaProfile): Renderer[Json.Node, JsonSchemaDocument] =
    JsonSchemaRenderer(Side.Write, profile)

  def apply(
      side: Side,
      profile: JsonSchemaProfile,
      namespaces: NonEmptyList[Metadata.Namespace] = JsonSchema.Namespaces
  ): Renderer[Json.Node, JsonSchemaDocument] =
    val renderer = JsonSchemaStateRenderer(
      namespaces,
      profile,
      JsonSchemaNodeRenderer(side, profile, namespaces, _)
    )

    Renderer: [w, r] =>
      (json: Json.Node[w, r]) =>
        val (context, rendered) = renderer.render(json).run(JsonSchemaContext.Empty).value
        val (root, definitions) = hoisted(profile, rendered, context.definitions)

        val dialect = profile.dialect.map(value => "$schema" -> CirceJson.fromString(value)).toList
        val defs = Option
          .when(definitions.nonEmpty)(profile.definitions.map(_ -> CirceJson.obj(definitions.toList*)))
          .flatten
          .toList

        val document = JsonSchema.merge(JsonSchema.merge(CirceJson.obj(dialect*), root), defs*)

        JsonSchemaDocument(document, context.issues.toList)

  /** Puts the root back where it was declared, unless it is reached again from inside itself.
    *
    * A named root goes through the same rules every other node does and comes back as a reference to its own
    * definition. That is correct and useless: a consumer asking for the schema of a document wants the document's
    * shape, not a pointer to it, and several will not accept a bare `$ref` at the top at all. So where nothing else
    * refers to the definition -- which is to say, where the root's own reference was the only one -- the body moves
    * back up and the definition goes away. A root that refers to itself keeps both, because there is no other way to
    * write it down.
    */
  private def hoisted(
      profile: JsonSchemaProfile,
      root: CirceJson,
      definitions: ListMap[String, CirceJson]
  ): (CirceJson, ListMap[String, CirceJson]) =
    val reference = for
      keyword <- profile.definitions
      pointer <- root.asObject.filter(_.size == 1).flatMap(_("$ref")).flatMap(_.asString)
      name <- definitions.keys.find(name => JsonSchema.ref(keyword, name) == root)
      if !definitions.values.exists(refers(_, pointer))
    yield name -> definitions(name)

    reference.fold((root, definitions))((name, body) => (body, definitions.removed(name)))

  /** Whether a document refers to `pointer` anywhere inside it. */
  private def refers(schema: CirceJson, pointer: String): Boolean = schema.asObject match
    case Some(fields) =>
      fields("$ref").flatMap(_.asString).contains(pointer) || fields.values.exists(refers(_, pointer))
    case None => schema.asArray.exists(_.exists(refers(_, pointer)))
