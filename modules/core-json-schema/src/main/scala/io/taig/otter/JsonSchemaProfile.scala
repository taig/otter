package io.taig.otter

import io.circe.Json as CirceJson
import io.taig.otter.codec.ConstraintJsonSchema

/** What a dialect, or a consumer's profile of one, decides about a document.
  *
  * Everything a JSON Schema renderer can honestly disagree with itself about. [[JsonSchemaProfile.Draft202012]] is the
  * vocabulary; [[JsonSchemaProfile.Strict]] is that vocabulary minus what a structured output consumer will not read,
  * plus what it insists on. Keeping the two behind this is what stops a third -- OpenAPI 3.1, a response schema for
  * some other model -- from having to copy the renderer.
  *
  * Data rather than a trait, because none of these decisions needs late binding: a profile is a record of answers, and
  * the three that read as behaviour -- how nothing is admitted, which constraints survive, which `format` names do --
  * are each a choice from a closed set, so they are carried as data too and applied by [[nullable]], [[keyword]] and
  * [[format]]. No parameter has a default: the questions are independent, and a fourth question added here should stop
  * every profile from compiling until it has an answer rather than silently receive one.
  */
final case class JsonSchemaProfile(
    /** The `$schema` a root document declares, if it declares one. */
    dialect: Option[String],

    /** Under which keyword a hoisted definition is declared, and therefore what a `$ref` points at.
      *
      * `None` inlines everything, which is what a consumer that will not follow a reference needs -- at the cost of not
      * terminating on a schema that refers to itself, which is reported rather than looped on.
      */
    definitions: Option[String],

    /** Whether a schema is allowed to refer to itself at all. */
    recursion: Boolean,

    /** What a record says about the keys it did not list. `None` says nothing, which is the truth: a record decoder
      * ignores what it does not recognise.
      */
    additionalProperties: Option[Boolean],

    /** Whether a key that may be absent is listed in `required` anyway, and allowed to hold nothing instead. */
    total: Boolean,

    /** Whether a positional array is expressible, or has to widen to a homogeneous one. */
    prefixItems: Boolean,

    /** Whether an object of keys the schema does not list is expressible. */
    dictionaries: Boolean,

    /** Whether the laxer wire forms a coercion accepts are worth saying, or the canonical one is the better answer. */
    coercion: Boolean,

    /** Whether a branch's name is worth a `title`. */
    branchTitles: Boolean,

    /** How a schema that also admits nothing says so. */
    nullability: JsonSchemaProfile.Nullability,

    /** Whether a schema's constraints contribute keywords, or the document says only what a value's shape is. */
    constraints: Boolean,

    /** Which `format` names survive. `None` keeps every one of them. */
    formats: Option[Set[String]]
):
  /** How a schema that also admits nothing says so. */
  def nullable(schema: CirceJson): CirceJson = nullability(schema)

  /** What a constraint contributes, or nothing where the profile has no counterpart for it. */
  def keyword(constraint: Constraint): Option[(String, CirceJson)] =
    if constraints then ConstraintJsonSchema.keyword(constraint) else None

  /** Which `format` a named text keeps. */
  def format(name: String): Option[String] = Option.when(formats.forall(_.contains(name)))(name)

object JsonSchemaProfile:
  /** How a schema that also admits nothing says so.
    *
    * [[Nullability.AnyOf]] is one rule with no case analysis, and it composes with a `$ref`, a `const` and an `enum`,
    * none of which [[Nullability.TypeArray]] can rewrite because none of them is a `type`. [[Nullability.TypeArray]]
    * reads better where it applies, and falls back to the other where it does not.
    */
  enum Nullability:
    case AnyOf, TypeArray

    def apply(schema: CirceJson): CirceJson = this match
      case Nullability.AnyOf     => JsonSchema.anyOf(JsonSchema.alternatives(schema) :+ JsonSchema.Null)
      case Nullability.TypeArray =>
        schema.asObject.flatMap(_("type")).flatMap(_.asString) match
          case Some(name) =>
            JsonSchema.merge(schema, "type" -> CirceJson.arr(CirceJson.fromString(name), CirceJson.fromString("null")))
          case None => Nullability.AnyOf(schema)

  /** The `format` names JSON Schema itself registers, which is what a strict consumer recognises. */
  val Formats: Set[String] =
    Set("date-time", "time", "date", "duration", "email", "hostname", "uri", "ipv4", "ipv6", "uuid")

  /** The full vocabulary, which is what a validator, a documentation generator or a tool schema that is not asked to be
    * strict reads.
    *
    * `format` is an open annotation vocabulary and a conforming validator ignores a name it does not know, so a name
    * the schema went to the trouble of giving is worth keeping: `isbn` and `zone-id` document what a string is even
    * where nothing checks it.
    */
  val Draft202012: JsonSchemaProfile = JsonSchemaProfile(
    dialect = Some("https://json-schema.org/draft/2020-12/schema"),
    definitions = Some("$defs"),
    recursion = true,
    additionalProperties = None,
    total = false,
    prefixItems = true,
    dictionaries = true,
    coercion = true,
    branchTitles = true,
    nullability = JsonSchemaProfile.Nullability.AnyOf,
    constraints = true,
    formats = None
  )

  /** What a strict structured output consumer accepts: no recursion, no numeric or string constraint, no positional
    * array, no open object, `additionalProperties: false` on every object and every property listed in `required`. Only
    * the registered `format` names survive.
    *
    * Everything it cannot say, it says nothing about, and records why. Dropping the constraints is exactly what the
    * Anthropic SDKs do -- strip what the consumer will not read and check it client side -- and otter's decoder is that
    * client side check, so the pairing is exact: the producer is told less, and the value is still refused if it breaks
    * a constraint.
    *
    * Written out rather than as a `copy` of [[Draft202012]], so that which decisions are independent is visible rather
    * than inferred from a diff.
    */
  val Strict: JsonSchemaProfile = JsonSchemaProfile(
    dialect = None,
    definitions = Some("$defs"),
    recursion = false,
    additionalProperties = Some(false),
    total = true,
    prefixItems = false,
    dictionaries = false,
    coercion = false,
    branchTitles = true,
    nullability = JsonSchemaProfile.Nullability.AnyOf,
    constraints = false,
    formats = Some(JsonSchemaProfile.Formats)
  )
