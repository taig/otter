package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Note
import io.taig.otter.fixture.Tree
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

/** The two sides of a schema, which is the whole reason a generator needs a [[io.taig.otter.Side]] at all.
  *
  * Every row here is read off the circe interpreters, which are what the wire actually is. A codec never has to ask
  * which side it is on, because it was handed either a value or a document; a renderer is handed neither.
  */
object JsonTypescriptEffectDirectionTest extends ZIOSpecDefault:
  private def read(schema: Json.Node[?, ?]): String =
    JsonTypescriptEffectRenderer.reader.render(schema).mkString("\n\n")

  private def write(schema: Json.Node[?, ?]): String =
    JsonTypescriptEffectRenderer.writer.render(schema).mkString("\n\n")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptEffectDirectionTest")(
    suite("field")(
      /** The key is dropped when the value is absent, so on the way out it is only ever missing. A lenient field, which
        * is what a field is unless it says otherwise, takes a missing key and an explicit null alike coming back.
        */
      test("an omitted field is optional out and optional or null in"):
        assertTrue(
          write(json.omittedTag) == """Schema.Struct({
                                      |  title: Schema.String,
                                      |  tag: Schema.optional(Schema.Int)
                                      |})""".stripMargin,
          read(json.omittedTag) == """Schema.Struct({
                                     |  title: Schema.String,
                                     |  tag: Schema.optionalWith(Schema.Int, { nullable: true })
                                     |})""".stripMargin
        )
      ,
      /** The key is always there, holding null when the value is absent. */
      test("a nullable field is required and null out, and optional or null in"):
        assertTrue(
          write(json.nullableTag) == """Schema.Struct({
                                       |  title: Schema.String,
                                       |  tag: Schema.NullOr(Schema.Int)
                                       |})""".stripMargin,
          read(json.nullableTag) == """Schema.Struct({
                                      |  title: Schema.String,
                                      |  tag: Schema.optionalWith(Schema.Int, { nullable: true })
                                      |})""".stripMargin
        )
      ,
      /** A strict field takes only the form it writes, which is exactly what makes the two sides agree again. */
      test("a strict field reads back only what it writes, so the sides agree"):
        val omitted = field("tag", int).optional.omitted.strict.toRecord
        val nulled = field("tag", int).optional.nullable.strict.toRecord

        assertTrue(
          read(omitted) == write(omitted),
          write(omitted) == """Schema.Struct({ tag: Schema.optional(Schema.Int) })""",
          read(nulled) == write(nulled),
          write(nulled) == """Schema.Struct({ tag: Schema.NullOr(Schema.Int) })"""
        )
      ,
      /** Two layers of absence: no key at all is the outer one, a null the inner. Only a strict field can tell them
        * apart, and the generated schema has to keep them apart too.
        */
      test("a strict field over an optional schema keeps both layers"):
        assertTrue(
          read(json.nestedTag) == write(json.nestedTag),
          write(json.nestedTag) == """Schema.Struct({ tag: Schema.optional(Schema.NullOr(Schema.Int)) })"""
        )
      ,
      /** A field holding a default is never absent when written, and may always be absent when read. The default is an
        * `Eval` of a Scala value and cannot be rendered, so the read side says the key may be missing and stops there.
        */
      test("a defaulted field is required out and optional in, without the default"):
        val schema = field("tag", int).optional(0).toRecord

        assertTrue(
          write(schema) == """Schema.Struct({ tag: Schema.Int })""",
          read(schema) == """Schema.Struct({ tag: Schema.optionalWith(Schema.Int, { nullable: true }) })"""
        )
      ,
      test("a defaulted schema is required out and nullable in"):
        val schema = field("tag", int.optional(0)).toRecord

        assertTrue(
          write(schema) == """Schema.Struct({ tag: Schema.Int })""",
          read(schema) == """Schema.Struct({ tag: Schema.NullOr(Schema.Int) })"""
        )
    ),
    suite("coerce")(
      /** The decoder normalises a quoted number or a stringified boolean before it looks at it, so the read side takes
        * forms the write side would never produce. The union is shared, because every coerced number takes the same
        * ones.
        */
      test("a coerced number widens on the way in only"):
        assertTrue(
          write(coerce(int)) == "Schema.Int",
          read(coerce(int)) == """export type CoerceNumber = Schema.Schema.Type<typeof CoerceNumber>;
                                 |
                                 |export const CoerceNumber = Schema.Union(Schema.Number, Schema.NumberFromString);
                                 |
                                 |CoerceNumber.pipe(Schema.int())""".stripMargin
        )
      ,
      /** A coercion replaces the node with a union of the forms it accepts, so whatever the node was claiming has to
        * come back as a filter -- integrality included, which is not a constraint but which number node it was.
        */
      test("a coercion keeps what the primitive underneath was claiming"):
        assertTrue(
          read(coerce(double)) == """export type CoerceNumber = Schema.Schema.Type<typeof CoerceNumber>;
                                    |
                                    |export const CoerceNumber = Schema.Union(Schema.Number, Schema.NumberFromString);
                                    |
                                    |CoerceNumber""".stripMargin
        )
      ,
      test("a coerced boolean and a coerced text"):
        assertTrue(
          write(coerce(boolean)) == "Schema.Boolean",
          write(coerce(string)) == "Schema.String",
          read(coerce(boolean)) == """export type CoerceBoolean = Schema.Schema.Type<typeof CoerceBoolean>;
                                     |
                                     |export const CoerceBoolean = Schema.Union(
                                     |  Schema.Boolean,
                                     |  Schema.transform(
                                     |    Schema.Union(Schema.Literal("true"), Schema.Literal("false")),
                                     |    Schema.Boolean,
                                     |    {
                                     |      decode: (value) => value === "true",
                                     |      encode: (value) => value ? "true" : "false"
                                     |    }
                                     |  )
                                     |);
                                     |
                                     |CoerceBoolean""".stripMargin,
          read(coerce(string)) == """export type CoerceString = Schema.Schema.Type<typeof CoerceString>;
                                    |
                                    |export const CoerceString = Schema.Union(
                                    |  Schema.String,
                                    |  Schema.transform(
                                    |    Schema.Number,
                                    |    Schema.String,
                                    |    {
                                    |      decode: (value) => String(value),
                                    |      encode: (value) => Number(value)
                                    |    }
                                    |  ),
                                    |  Schema.transform(
                                    |    Schema.Boolean,
                                    |    Schema.String,
                                    |    {
                                    |      decode: (value) => value ? "true" : "false",
                                    |      encode: (value) => value === "true"
                                    |    }
                                    |  )
                                    |);
                                    |
                                    |CoerceString""".stripMargin
        )
    ),
    suite("agreement")(
      /** The property [[JsonTypescriptEffectRenderer.Naming.Collapsed]] rests on: a schema with nothing asymmetric in
        * it says the same thing in both directions, and there is then nothing to be gained from two names.
        */
      test("a schema with no absence, default or coercion says the same thing twice"):
        lazy val tree: Json.Record[Tree] =
          (field("value", int) :* field("children", collection.list(tree))).to[Tree].attr(Keys.name, "Tree")

        assertTrue(
          read(json.book) == write(json.book),
          read(json.shape) == write(json.shape),
          read(json.genre) == write(json.genre),
          read(json.flatBook) == write(json.flatBook),
          read(tree) == write(tree)
        )
      ,
      test("and a schema with one of them does not"):
        assertTrue(
          read(json.omittedTag) != write(json.omittedTag),
          read(json.nullableTag) != write(json.nullableTag),
          read(coerce(int)) != write(coerce(int))
        )
      ,
      /** A renderer takes no value, so a schema that has lost one of its directions still has both shapes. Which
        * direction a text format was built for is a type level fact and invisible here, so all four constructors render
        * alike.
        */
      test("a one directional schema renders on both sides"):
        assertTrue(
          read(json.title) == "Schema.String",
          write(json.title) == "Schema.String",
          read(json.isbn) == "Schema.String",
          write(json.isbn) == "Schema.String",
          read(json.trimmedNote) == read(json.omittedTag),
          write(json.trimmedNote) == write(json.omittedTag)
        )
    ),
    suite("wire")(
      /** The generated write shape is a claim about what circe encodes, so it is worth asking circe. An omitted field
        * drops its key and a nullable one keeps it holding null, and the generated schemas say `optional` and `NullOr`
        * in exactly those two places.
        */
      test("what the write side calls optional is the key circe drops"):
        val absent = JsonCirceEncoder.encode(json.omittedTag, Note("Dune", none))
        val present = JsonCirceEncoder.encode(json.omittedTag, Note("Dune", 1.some))

        assertTrue(
          absent.asObject.map(_.keys.toList) == List("title").some,
          present.asObject.map(_.keys.toList) == List("title", "tag").some,
          write(json.omittedTag).contains("""Schema.optional(Schema.Int)""")
        )
      ,
      test("what the write side calls nullable is the key circe fills with null"):
        val absent = JsonCirceEncoder.encode(json.nullableTag, Note("Dune", none))

        assertTrue(
          absent.asObject.flatMap(_.apply("tag")) == CirceJson.Null.some,
          write(json.nullableTag).contains("""Schema.NullOr(Schema.Int)""")
        )
      ,
      /** Both documents the encoder can produce are accepted by the decoder, which is what makes the read side of a
        * lenient field the union of the two, whichever way it was written.
        */
      test("the read side is lax because the decoder is"):
        val omitted = CirceJson.obj("title" -> CirceJson.fromString("Dune"))
        val nulled = CirceJson.obj("title" -> CirceJson.fromString("Dune"), "tag" -> CirceJson.Null)

        assertTrue(
          JsonCirceDecoder.decode(json.omittedTag, omitted).isValid,
          JsonCirceDecoder.decode(json.omittedTag, nulled).isValid,
          read(json.omittedTag).contains("""Schema.optionalWith(Schema.Int, { nullable: true })""")
        )
      ,
      /** A strict omitted field is the one that refuses the null, and it is also the one whose two sides agree. */
      test("a strict field refuses what the lenient one took, and its sides agree"):
        val schema = field("title", string) :* field("tag", int).optional.omitted.strict
        val nulled = CirceJson.obj("title" -> CirceJson.fromString("Dune"), "tag" -> CirceJson.Null)

        assertTrue(
          JsonCirceDecoder.decode(schema.to[Note], nulled).isValid == false,
          read(schema) == write(schema)
        )
    )
  )
