package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Side
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

/** The structural type of a schema, which is what a recursive declaration has to be told because TypeScript will not
  * infer through a cycle.
  */
object JsonTypescriptTypeRendererTest extends ZIOSpecDefault:
  private def read(schema: Json.Node[?, ?]): String = JsonTypescriptTypeRenderer(Side.Read).render(schema).render

  private def write(schema: Json.Node[?, ?]): String = JsonTypescriptTypeRenderer(Side.Write).render(schema).render

  private def both(schema: Json.Node[?, ?]): String = read(schema)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptTypeRendererTest")(
    test("a record names its members"):
      assertTrue(
        read(json.book) == write(json.book),
        both(json.book) == """{
                             |  title: string;
                             |  pages: number;
                             |  read: boolean;
                             |}""".stripMargin
      )
    ,
    test("an empty record is an empty object"):
      assertTrue(both(RNil) == "{}")
    ,
    test("a collection, a dictionary and a tuple"):
      assertTrue(
        both(collection.list(int)) == "ReadonlyArray<number>",
        both(dictionary.map(boolean)) == "Record<string, boolean>",
        both(TNil :* string :* int) == "[string, number]"
      )
    ,
    test("an enumeration and a constant are literal types"):
      assertTrue(
        both(json.genre) == """"fiction" | "history" | "poetry"""",
        both(constant(string, "foobar")) == """"foobar""""
      )
    ,
    /** A branch's name is only an error label -- the encoder never writes it -- so a union is untagged. */
    test("a union is untagged, and breaks once a member has"):
      assertTrue(both(json.shape) == """#| { radius: number }
                                        #| { side: number }
                                        #| {
                                        #    base: number;
                                        #    height: number;
                                        #  }""".stripMargin('#'))
    ,
    /** The key is dropped when the value is absent, so on the way out it is only ever missing. Coming back in, a
      * lenient field takes a missing key and an explicit null alike.
      */
    test("an omitted field reads laxer than it writes"):
      assertTrue(
        write(json.omittedTag) == """{
                                    |  title: string;
                                    |  tag?: number | undefined;
                                    |}""".stripMargin,
        read(json.omittedTag) == """{
                                   |  title: string;
                                   |  tag?: number | null | undefined;
                                   |}""".stripMargin
      )
    ,
    /** The key is always written, holding null when the value is absent. */
    test("a nullable field reads laxer than it writes"):
      assertTrue(
        write(json.nullableTag) == """{
                                     |  title: string;
                                     |  tag: number | null;
                                     |}""".stripMargin,
        read(json.nullableTag) == """{
                                    |  title: string;
                                    |  tag?: number | null | undefined;
                                    |}""".stripMargin
      )
    ,
    /** A strict field takes only the form it writes, which is what makes the two sides agree again. */
    test("a strict field reads exactly what it writes"):
      val omitted = field("tag", int).optional.omitted.strict.toRecord
      val nulled = field("tag", int).optional.nullable.strict.toRecord

      assertTrue(
        read(omitted) == write(omitted),
        both(omitted) == """{ tag?: number | undefined }""",
        read(nulled) == write(nulled),
        both(nulled) == """{ tag: number | null }"""
      )
    ,
    /** Two layers of absence, which only a strict field can tell apart: no key at all is the outer one, a null the
      * inner.
      */
    test("a strict field over an optional schema keeps the layers apart"):
      assertTrue(
        read(json.nestedTag) == write(json.nestedTag),
        both(json.nestedTag) == """{ tag?: number | null | undefined }"""
      )
    ,
    /** A field holding a default is never absent when written, and may always be absent when read. */
    test("a defaulted field is required on the way out and optional on the way in"):
      val schema = field("tag", int).optional(0).toRecord

      assertTrue(
        write(schema) == """{ tag: number }""",
        read(schema) == """{ tag?: number | null | undefined }"""
      )
    ,
    test("a defaulted schema is required on the way out and nullable on the way in"):
      val schema = field("tag", int.optional(0)).toRecord

      assertTrue(
        write(schema) == """{ tag: number }""",
        read(schema) == """{ tag: number | null }"""
      )
    ,
    /** The decoder normalises a quoted number and a stringified boolean before it looks at them, so the read side
      * accepts forms the write side would never produce.
      */
    test("a coercion widens the read side only"):
      assertTrue(
        write(coerce(int)) == "number",
        read(coerce(int)) == "number | string",
        write(coerce(string)) == "string",
        read(coerce(string)) == "string | number | boolean",
        read(coerce(boolean)) == "boolean | string"
      )
    ,
    /** A renderer involves no value, so a schema that has lost one of its directions still has a shape. */
    test("a one directional schema still has a type on both sides"):
      assertTrue(
        both(json.title) == "string",
        both(json.isbn) == "string",
        both(json.trimmed) == "string"
      )
  )
