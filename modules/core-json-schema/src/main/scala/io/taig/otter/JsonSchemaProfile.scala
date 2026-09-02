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
  * `JsonTypescriptTarget` has three methods because a TypeScript target varies in three words. This has more because
  * "which JSON Schema" is a question about a consumer rather than about a language, and consumers differ in more than
  * syntax.
  */
trait JsonSchemaProfile:
  /** The `$schema` a root document declares, if it declares one. */
  def dialect: Option[String]

  /** Under which keyword a hoisted definition is declared, and therefore what a `$ref` points at.
    *
    * `None` inlines everything, which is what a consumer that will not follow a reference needs -- at the cost of not
    * terminating on a schema that refers to itself, which is reported rather than looped on.
    */
  def definitions: Option[String]

  /** Whether a schema is allowed to refer to itself at all. */
  def recursion: Boolean

  /** What a record says about the keys it did not list. `None` says nothing, which is the truth: a record decoder
    * ignores what it does not recognise.
    */
  def additionalProperties: Option[Boolean]

  /** Whether a key that may be absent is listed in `required` anyway, and allowed to hold nothing instead. */
  def total: Boolean

  /** Whether a positional array is expressible, or has to widen to a homogeneous one. */
  def prefixItems: Boolean

  /** Whether an object of keys the schema does not list is expressible. */
  def dictionaries: Boolean

  /** Whether the laxer wire forms a coercion accepts are worth saying, or the canonical one is the better answer. */
  def coercion: Boolean

  /** Whether a branch's name is worth a `title`. */
  def branchTitles: Boolean

  /** How a schema that also admits nothing says so. */
  def nullable(schema: CirceJson): CirceJson

  /** What a constraint contributes, or nothing where the profile has no counterpart for it. */
  def keyword(constraint: Constraint): Option[(String, CirceJson)]

  /** Which `format` a named text keeps. */
  def format(name: String): Option[String]

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
    */
  val Draft202012: JsonSchemaProfile = new JsonSchemaProfile:
    override val dialect: Option[String] = Some("https://json-schema.org/draft/2020-12/schema")
    override val definitions: Option[String] = Some("$defs")
    override val recursion: Boolean = true
    override val additionalProperties: Option[Boolean] = None
    override val total: Boolean = false
    override val prefixItems: Boolean = true
    override val dictionaries: Boolean = true
    override val coercion: Boolean = true
    override val branchTitles: Boolean = true
    override def nullable(schema: CirceJson): CirceJson = JsonSchemaProfile.Nullability.AnyOf(schema)
    override def keyword(constraint: Constraint): Option[(String, CirceJson)] = ConstraintJsonSchema.keyword(constraint)

    /** `format` is an open annotation vocabulary and a conforming validator ignores a name it does not know, so a name
      * the schema went to the trouble of giving is worth keeping: `isbn` and `zone-id` document what a string is even
      * where nothing checks it.
      */
    override def format(name: String): Option[String] = Some(name)

  /** What a strict structured output consumer accepts: no recursion, no numeric or string constraint, no positional
    * array, no open object, `additionalProperties: false` on every object and every property listed in `required`. Only
    * the registered `format` names survive.
    *
    * Everything it cannot say, it says nothing about, and records why. Dropping the constraints is exactly what the
    * Anthropic SDKs do -- strip what the consumer will not read and check it client side -- and otter's decoder is that
    * client side check, so the pairing is exact: the producer is told less, and the value is still refused if it breaks
    * a constraint.
    *
    * Written out rather than as an override of [[Draft202012]], so that which decisions are independent is visible
    * rather than inferred from a diff.
    */
  val Strict: JsonSchemaProfile = new JsonSchemaProfile:
    override val dialect: Option[String] = None
    override val definitions: Option[String] = Some("$defs")
    override val recursion: Boolean = false
    override val additionalProperties: Option[Boolean] = Some(false)
    override val total: Boolean = true
    override val prefixItems: Boolean = false
    override val dictionaries: Boolean = false
    override val coercion: Boolean = false
    override val branchTitles: Boolean = true
    override def nullable(schema: CirceJson): CirceJson = JsonSchemaProfile.Nullability.AnyOf(schema)
    override def keyword(constraint: Constraint): Option[(String, CirceJson)] = None
    override def format(name: String): Option[String] = Option.when(JsonSchemaProfile.Formats.contains(name))(name)
