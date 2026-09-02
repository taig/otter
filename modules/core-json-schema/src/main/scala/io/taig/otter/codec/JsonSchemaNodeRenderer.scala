package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Coerce
import io.taig.otter.Collection
import io.taig.otter.Constraint
import io.taig.otter.Dictionary
import io.taig.otter.Json
import io.taig.otter.JsonSchema
import io.taig.otter.JsonSchemaIssue
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Metadata
import io.taig.otter.Optional
import io.taig.otter.Primitive
import io.taig.otter.Side
import io.taig.otter.Tuple

/** Describes one side of a JSON schema as a JSON Schema document.
  *
  * The same dispatch on the alphabet the circe interpreters do, with two differences. No value takes part, so the side
  * has to be given rather than implied. And a name may have to be hoisted out to a definition of its own, which is why
  * every child goes through `renderer` -- the fixpoint -- and why the result is a `State` rather than a document.
  *
  * What the profile cannot say is left unsaid and recorded, never approximated. The safe direction is to constrain less
  * than the decoder: a document that admits more than the decoder takes rejects nothing the decoder accepts.
  */
final class JsonSchemaNodeRenderer(
    side: Side,
    profile: JsonSchemaProfile,
    namespaces: NonEmptyList[Metadata.Namespace],
    renderer: Renderer[Json.Node, State[JsonSchemaContext, CirceJson]]
) extends Renderer[Json.Node, State[JsonSchemaContext, CirceJson]]:
  override def render[W, R](json: Json.Node[W, R]): State[JsonSchemaContext, CirceJson] = json match
    case Json.Coerce.Schema(node)     => coerce(node.self)
    case Json.Collection.Schema(node) =>
      child(node.self.schema.value).flatMap: items =>
        keywords(collection(node.self)).map: keywords =>
          JsonSchema.merge(JsonSchema.merge(JsonSchema.typed("array"), "items" -> items), keywords*)
    case Json.Constant.Schema(node) =>
      child(node.self.schema.value).map(JsonSchema.merge(_, "const" -> JsonSchemaLiteral.constant(node.self)))
    case Json.Dictionary.Schema(node)  => dictionary(node.self)
    case Json.Enumeration.Schema(node) =>
      child(node.self.schema.value).map: primitive =>
        val values = JsonSchemaLiteral.enumeration(node.self)
        JsonSchema.merge(primitive, "enum" -> CirceJson.fromValues(values.toList))
    case Json.Optional.Schema(node)               => optional(node.self)
    case Json.Primitive.Boolean.Schema(_)         => JsonSchema.typed("boolean").pure
    case Json.Primitive.Number.Schema(annotation) => number(annotation.self)
    case Json.Primitive.Text.Schema(annotation)   => text(annotation.self)
    case Json.Record.Schema(node)                 =>
      node.self.fields.toList.traverse(reference => field(reference.value)).map(record)
    case Json.Tuple.Schema(node) => tuple(node.self)
    case Json.Union.Schema(node) =>
      node.self.branches.toNonEmptyList.traverse(reference => branch(reference.value)).map(JsonSchema.anyOf)

  private def child(json: Json.Node[?, ?]): State[JsonSchemaContext, CirceJson] = renderer.render(json)

  private def issue(f: Option[String] => JsonSchemaIssue): State[JsonSchemaContext, Unit] =
    State.modify(_.issue(f))

  /** What the profile keeps of a schema's constraints, recording every one it has no keyword for. */
  private def keywords(constraints: Chain[Constraint]): State[JsonSchemaContext, List[(String, CirceJson)]] =
    constraints.toList
      .traverse: constraint =>
        profile.keyword(constraint) match
          case Some(keyword) => keyword.some.pure[[a] =>> State[JsonSchemaContext, a]]
          case None          => issue(JsonSchemaIssue.Dropped(_, constraint)).as(none[(String, CirceJson)])
      .map(_.flatten)

  /* -- records ------------------------------------------------------------------------------------------------- */

  /** A record's member: its key, what it holds, and whether the key has to be there.
    *
    * Under a total profile a key that may be absent is listed anyway and allowed to hold nothing instead. That is a
    * narrowing a lenient field takes in its stride -- it reads a missing key and an explicit empty alike -- and one a
    * strict field that drops its key does not, which is the one place the narrowing is not safe.
    */
  private def field(json: Json.Field.Node[?, ?]): State[JsonSchemaContext, (String, CirceJson, Boolean)] =
    val name = json.self.self.name

    child(json.self.self.schema.value).flatMap: schema =>
      val annotated = JsonSchemaAnnotation(namespaces, json.self.metadata, schema)

      Json.presence(side, json.self.metadata, json.self.self) match
        case Json.Presence.Required => (name, annotated, true).pure
        case Json.Presence.Nullable => (name, profile.nullable(annotated), true).pure
        case Json.Presence.Optional =>
          if !profile.total then (name, annotated, false).pure
          else issue(JsonSchemaIssue.Total(_, name)).as((name, profile.nullable(annotated), true))
        case Json.Presence.OptionalNullable =>
          (name, profile.nullable(annotated), profile.total).pure

  private def record(fields: List[(String, CirceJson, Boolean)]): CirceJson =
    val properties = CirceJson.obj(fields.map((name, schema, _) => name -> schema)*)
    val required = fields.collect { case (name, _, true) => CirceJson.fromString(name) }

    JsonSchema.merge(
      JsonSchema.typed("object"),
      List(
        "properties" -> properties.some,
        "required" -> CirceJson.fromValues(required).some,
        "additionalProperties" -> profile.additionalProperties.map(CirceJson.fromBoolean)
      ).collect { case (key, Some(value)) => key -> value }*
    )

  /* -- unions -------------------------------------------------------------------------------------------------- */

  /** A branch, labelled by the name it carries where the profile wants one.
    *
    * Nothing on the wire says which branch a document belongs to, and the name is never written out, so `title` -- the
    * one slot JSON Schema has for a label nothing reads back -- is where it belongs. A validator ignores it and a
    * producer choosing between branches does not.
    */
  private def branch(json: Json.Branch.Node[?, ?]): State[JsonSchemaContext, CirceJson] =
    child(json.self.self.schema.value).map: schema =>
      val titled = profile.branchTitles && !schema.asObject.exists(_.contains("title"))

      if titled then JsonSchema.merge(CirceJson.obj("title" -> CirceJson.fromString(json.self.self.name)), schema)
      else schema

  /* -- values -------------------------------------------------------------------------------------------------- */

  private def optional[W, R](schema: Optional[Json.Node, W, R]): State[JsonSchemaContext, CirceJson] = schema match
    case Optional.Modify(self, _, _)    => optional(self)
    case Optional.Root(reference)       => child(reference.value).map(profile.nullable)
    case Optional.Default(reference, _) =>
      side match
        case Side.Write => child(reference.value)
        case Side.Read  => child(reference.value).map(profile.nullable)

  /** The laxer wire forms the decoder normalises before handing over, which only the read side sees.
    *
    * Telling a producer "a number, or a string that parses as one" is strictly worse than telling it "a number" where
    * the producer is writing the document to order rather than replaying a legacy one, so a profile is allowed to say
    * only what the schema writes and record that it did.
    */
  private def coerce[W, R](schema: Coerce[Json.Primitive.Node, W, R]): State[JsonSchemaContext, CirceJson] =
    schema match
      case Coerce.Modify(self, _, _) => coerce(self)
      case Coerce.Root(reference)    =>
        side match
          case Side.Write                     => child(reference.value)
          case Side.Read if !profile.coercion =>
            issue(JsonSchemaIssue.Coerced.apply) *> child(reference.value)
          case Side.Read =>
            child(reference.value).map: primitive =>
              val laxer = reference.value match
                case Json.Primitive.Boolean.Schema(_) => List(JsonSchema.typed("string"))
                case Json.Primitive.Number.Schema(_)  => List(JsonSchema.typed("string"))
                case Json.Primitive.Text.Schema(_)    => List(JsonSchema.typed("number"), JsonSchema.typed("boolean"))

              JsonSchema.anyOf(JsonSchema.alternatives(primitive) ++ laxer)

  /* -- collections --------------------------------------------------------------------------------------------- */

  /** An object of keys the schema does not list.
    *
    * The key schema is text whatever it carries, so what it says beyond that -- a length, a pattern, a format -- is
    * `propertyNames`. What it *does* is invisible: a key schema that parses is two opaque functions and a name, and a
    * `propertyNames` can only repeat the name.
    */
  private def dictionary[W, R](
      schema: Dictionary[Json.Primitive.Text.Node, Json.Node, W, R]
  ): State[JsonSchemaContext, CirceJson] =
    if !profile.dictionaries then issue(JsonSchemaIssue.Open.apply).as(JsonSchema.typed("object"))
    else
      child(schema.schema.value).flatMap: values =>
        child(schema.key.value).flatMap: keys =>
          keywords(dictionaries(schema)).map: keywords =>
            /* A key that says no more than "it is text" says nothing a dictionary does not already say. */
            val names = Option.when(keys.asObject.exists(_.size > 1))("propertyNames" -> keys)

            JsonSchema.merge(
              JsonSchema.merge(JsonSchema.typed("object"), "additionalProperties" -> values),
              names.toList ++ keywords*
            )

  /** A fixed length, positionally typed array.
    *
    * `prefixItems` alone does not close a tuple in 2020-12, so the bounds go alongside it. Where a profile has no
    * positional vocabulary the array widens to the alternation of its positions, which is the one place a document
    * comes out admitting more than the decoder takes: arity stops being said at all.
    */
  private def tuple[W, R](schema: Tuple[Json.Node, W, R]): State[JsonSchemaContext, CirceJson] =
    schema.schemas.toList
      .traverse(reference => child(reference.value))
      .flatMap: elements =>
        if elements.isEmpty then JsonSchema.merge(JsonSchema.typed("array"), "maxItems" -> CirceJson.fromInt(0)).pure
        else if !profile.prefixItems then
          issue(JsonSchemaIssue.Positional.apply).as(
            JsonSchema
              .merge(JsonSchema.typed("array"), "items" -> JsonSchema.anyOf(NonEmptyList.fromListUnsafe(elements)))
          )
        else
          val arity = CirceJson.fromInt(elements.length)

          val self = JsonSchema.merge(
            JsonSchema.typed("array"),
            "prefixItems" -> CirceJson.fromValues(elements),
            "items" -> CirceJson.False,
            "minItems" -> arity,
            "maxItems" -> arity
          )

          /* Every position has to admit the empty form for the whole array to, so the alternative is the whole tuple
           * over again rather than a hole in this one. */
          if !Json.absent(side, schema) then self.pure
          else
            val empty = JsonSchema.merge(
              JsonSchema.typed("array"),
              "prefixItems" -> CirceJson.fromValues(elements.map(_ => JsonSchema.Null)),
              "items" -> CirceJson.False,
              "minItems" -> arity,
              "maxItems" -> arity
            )

            JsonSchema.anyOf(NonEmptyList.of(self, empty)).pure

  /* -- primitives ---------------------------------------------------------------------------------------------- */

  private def number[W, R](schema: Primitive.Number[W, R]): State[JsonSchemaContext, CirceJson] =
    keywords(number(schema, Chain.empty)).map(JsonSchema.merge(JsonSchema.typed(numeric(schema)), _*))

  /** An integral number is a number the decoder refuses when it has a fraction, which is what JSON Schema's `integer`
    * means as well, so the two agree and no separate keyword is needed.
    */
  private def numeric[W, R](schema: Primitive.Number[W, R]): String = schema match
    case Primitive.Number.BigDecimal(_)      => "number"
    case Primitive.Number.BigInteger(_)      => "integer"
    case Primitive.Number.Double(_)          => "number"
    case Primitive.Number.Float(_)           => "number"
    case Primitive.Number.Int(_)             => "integer"
    case Primitive.Number.Long(_)            => "integer"
    case Primitive.Number.Modify(self, _, _) => numeric(self)

  private def number[W, R](schema: Primitive.Number[W, R], constraints: Chain[Constraint]): Chain[Constraint] =
    schema match
      case Primitive.Number.BigDecimal(validation) => constraints ++ validation.constraints
      case Primitive.Number.BigInteger(validation) => constraints ++ validation.constraints
      case Primitive.Number.Double(validation)     => constraints ++ validation.constraints
      case Primitive.Number.Float(validation)      => constraints ++ validation.constraints
      case Primitive.Number.Int(validation)        => constraints ++ validation.constraints
      case Primitive.Number.Long(validation)       => constraints ++ validation.constraints
      case Primitive.Number.Modify(self, _, _)     => number(self, constraints)

  /** Every text is a string on the wire, whatever it parses into. What a named conversion adds is a `format`, and only
    * where the profile's consumer recognises the name: an unknown `format` is an annotation a conforming validator
    * ignores, but not every consumer is conforming, so a profile is allowed to insist on the registered set.
    */
  private def text[W, R](schema: Primitive.Text[W, R]): State[JsonSchemaContext, CirceJson] =
    format(schema).flatMap: format =>
      keywords(text(schema, Chain.empty)).map: keywords =>
        JsonSchema.merge(JsonSchema.merge(JsonSchema.typed("string"), format.toList*), keywords*)

  private def format[W, R](schema: Primitive.Text[W, R]): State[JsonSchemaContext, Option[(String, CirceJson)]] =
    schema match
      case Primitive.Text.Root(_)            => none.pure
      case Primitive.Text.Modify(self, _, _) => format(self)
      case Primitive.Text.Format(name, _, _) =>
        profile.format(name) match
          case Some(name) => ("format" -> CirceJson.fromString(name)).some.pure
          case None       => issue(JsonSchemaIssue.Format(_, name)).as(none)

  private def text[W, R](schema: Primitive.Text[W, R], constraints: Chain[Constraint]): Chain[Constraint] =
    schema match
      case Primitive.Text.Root(validation)   => constraints ++ validation.constraints
      case Primitive.Text.Format(_, _, _)    => constraints
      case Primitive.Text.Modify(self, _, _) => text(self, constraints)

  private def collection[W, R](schema: Collection[Json.Node, W, R]): Chain[Constraint] = schema match
    case Collection.Chained(_, validation) => validation.constraints
    case Collection.Indexed(_, validation) => validation.constraints
    case Collection.Linked(_, validation)  => validation.constraints
    case Collection.Modify(self, _, _)     => collection(self)

  private def dictionaries[W, R](
      schema: Dictionary[Json.Primitive.Text.Node, Json.Node, W, R]
  ): Chain[Constraint] = schema match
    case Dictionary.Hashed(_, _, _, validation) => validation.constraints
    case Dictionary.Linked(_, _, validation)    => validation.constraints
    case Dictionary.Modify(self, _, _)          => dictionaries(self)
