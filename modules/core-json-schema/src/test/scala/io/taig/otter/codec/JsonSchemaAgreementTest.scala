package io.taig.otter.codec

import cats.data.Validated
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Side
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** The claim the module exists to make: the document a producer is held to and the decoder its answer is read by come
  * from one schema and cannot drift.
  *
  * There is no JSON Schema validator on the classpath, so nothing here asserts that a document *validates* an instance.
  * What it asserts instead is the agreement that actually goes wrong when a schema is written by hand beside a parser:
  * which keys are mandatory. A key the document requires and the encoder never writes, or a key the decoder insists on
  * and the document leaves out, is the bug this pairing exists to make impossible.
  */
object JsonSchemaAgreementTest extends ZIOSpecDefault:
  private def document(side: Side, schema: Json.Node[?, ?]): CirceJson =
    JsonSchemaRenderer(side, JsonSchemaProfile.Draft202012).render(schema).value

  private def properties(document: CirceJson): Set[String] =
    document.asObject.flatMap(_("properties")).flatMap(_.asObject).map(_.keys.toSet).getOrElse(Set.empty)

  private def required(document: CirceJson): Set[String] =
    document.asObject
      .flatMap(_("required"))
      .flatMap(_.asArray)
      .map(_.flatMap(_.asString).toSet)
      .getOrElse(Set.empty)

  private def keys(json: CirceJson): Set[String] = json.asObject.map(_.keys.toSet).getOrElse(Set.empty)

  private val book = Book("Dune", 412, read = true)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaAgreementTest")(
    test("what the write side requires is exactly what the encoder writes"):
      val written = JsonCirceEncoder.encode(json.book, book)
      val schema = document(Side.Write, json.book)

      assertTrue(properties(schema) == keys(written), required(schema) == keys(written))
    ,
    test("a field the encoder drops is described but not required"):
      val written = JsonCirceEncoder.encode(json.omittedTag, Note("Dune", None))
      val schema = document(Side.Write, json.omittedTag)

      assertTrue(
        keys(written) == Set("title"),
        properties(schema) == Set("title", "tag"),
        required(schema) == keys(written)
      )
    ,
    test("a field the encoder writes as null is required, because the key is always there"):
      val written = JsonCirceEncoder.encode(json.nullableTag, Note("Dune", None))
      val schema = document(Side.Write, json.nullableTag)

      assertTrue(
        keys(written) == Set("title", "tag"),
        required(schema) == keys(written)
      )
    ,
    test("what the strict reader asks a producer for, the decoder reads"):
      val schema = JsonSchemaRenderer.reader(JsonSchemaProfile.Strict).render(json.book)

      /* Every key listed, every object closed: what a strict structured output consumer is handed. */
      val answer = CirceJson.obj("title" -> "Dune".asJson, "pages" -> 412.asJson, "read" -> true.asJson)

      assertTrue(
        schema.toEither.isRight,
        required(schema.value) == properties(schema.value),
        schema.value.asObject.flatMap(_("additionalProperties")).contains(CirceJson.False),
        JsonCirceDecoder.decode(json.book, answer) == Validated.valid(book)
      )
    ,
    test("a producer told to send null for an absent key is understood, which is what makes the narrowing safe"):
      val schema = JsonSchemaRenderer.reader(JsonSchemaProfile.Strict).render(json.omittedTag)

      val answer = CirceJson.obj("title" -> "Dune".asJson, "tag" -> CirceJson.Null)

      assertTrue(
        schema.issues.isEmpty,
        required(schema.value) == Set("title", "tag"),
        JsonCirceDecoder.decode(json.omittedTag, answer) == Validated.valid(Note("Dune", None))
      )
    ,
    test("every value an enumeration offers a producer is one the decoder takes"):
      val schema = JsonSchemaRenderer.reader(JsonSchemaProfile.Strict).render(json.genre)

      val values = schema.value.asObject
        .flatMap(_("enum"))
        .flatMap(_.asArray)
        .map(_.toList)
        .getOrElse(Nil)

      assertTrue(
        values.length == 3,
        values.forall(value => JsonCirceDecoder.decode(json.genre, value).isValid)
      )
    ,
    test("every branch a union offers a producer is one the decoder resolves"):
      val schema = JsonSchemaRenderer.reader(JsonSchemaProfile.Strict).render(json.shape)

      val branches = schema.value.asObject.flatMap(_("anyOf")).flatMap(_.asArray).map(_.toList).getOrElse(Nil)

      val instances = List(
        CirceJson.obj("radius" -> 1.0.asJson),
        CirceJson.obj("side" -> 2.0.asJson),
        CirceJson.obj("base" -> 3.0.asJson, "height" -> 4.0.asJson)
      )

      assertTrue(
        branches.length == 3,
        branches.map(required) == instances.map(keys),
        instances.forall(instance => JsonCirceDecoder.decode(json.shape, instance).isValid)
      )
  )
