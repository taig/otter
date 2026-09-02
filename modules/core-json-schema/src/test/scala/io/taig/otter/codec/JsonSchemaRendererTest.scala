package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

object JsonSchemaRendererTest extends ZIOSpecDefault:
  private def render(schema: Json.Node[?, ?]): String =
    JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012).render(schema).value.spaces2

  /** For a document holding an empty object, which `spaces2` prints with whitespace no source file may carry. */
  private def compact(schema: Json.Node[?, ?]): String =
    JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012).render(schema).value.noSpaces

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaRendererTest")(
    test("a primitive is its type"):
      assertTrue(
        render(boolean) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "boolean"
            |}""".stripMargin,
        render(int) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "integer"
            |}""".stripMargin,
        render(double) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "number"
            |}""".stripMargin,
        render(string) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "string"
            |}""".stripMargin
      )
    ,
    test("a named conversion is a format"):
      assertTrue(
        render(uuid) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "string",
            |  "format" : "uuid"
            |}""".stripMargin
      )
    ,
    test("a record is an object, and every member of it is required"):
      assertTrue(
        render(json.book) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "object",
            |  "properties" : {
            |    "title" : {
            |      "type" : "string"
            |    },
            |    "pages" : {
            |      "type" : "integer"
            |    },
            |    "read" : {
            |      "type" : "boolean"
            |    }
            |  },
            |  "required" : [
            |    "title",
            |    "pages",
            |    "read"
            |  ]
            |}""".stripMargin
      )
    ,
    test("an empty record says only that it is an object"):
      assertTrue(
        compact(RNil.toRecord) ==
          """{"$schema":"https://json-schema.org/draft/2020-12/schema",""" +
          """"type":"object","properties":{},"required":[]}"""
      )
    ,
    test("a collection is an array of one schema"):
      assertTrue(
        render(collection.list(int)) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "array",
            |  "items" : {
            |    "type" : "integer"
            |  }
            |}""".stripMargin
      )
    ,
    test("a tuple is a closed positional array"):
      assertTrue(
        render((TNil :* string :* int)) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "array",
            |  "prefixItems" : [
            |    {
            |      "type" : "string"
            |    },
            |    {
            |      "type" : "integer"
            |    }
            |  ],
            |  "items" : false,
            |  "minItems" : 2,
            |  "maxItems" : 2
            |}""".stripMargin
      )
    ,
    test("an enumeration is the values it admits, at the type underneath"):
      assertTrue(
        render(json.genre) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "string",
            |  "enum" : [
            |    "fiction",
            |    "history",
            |    "poetry"
            |  ]
            |}""".stripMargin
      )
    ,
    test("a constant is the one value it admits"):
      assertTrue(
        render(constant(string, "circle")) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "string",
            |  "const" : "circle"
            |}""".stripMargin
      )
    ,
    test("a union is an alternation, and a branch's name labels it"):
      assertTrue(
        render(json.shape) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "anyOf" : [
            |    {
            |      "title" : "circle",
            |      "type" : "object",
            |      "properties" : {
            |        "radius" : {
            |          "type" : "number"
            |        }
            |      },
            |      "required" : [
            |        "radius"
            |      ]
            |    },
            |    {
            |      "title" : "square",
            |      "type" : "object",
            |      "properties" : {
            |        "side" : {
            |          "type" : "number"
            |        }
            |      },
            |      "required" : [
            |        "side"
            |      ]
            |    },
            |    {
            |      "title" : "triangle",
            |      "type" : "object",
            |      "properties" : {
            |        "base" : {
            |          "type" : "number"
            |        },
            |        "height" : {
            |          "type" : "number"
            |        }
            |      },
            |      "required" : [
            |        "base",
            |        "height"
            |      ]
            |    }
            |  ]
            |}""".stripMargin
      )
    ,
    test("a dictionary is an object of one schema, and a key that says more says it in propertyNames"):
      assertTrue(
        render(dictionary.map(string, int)) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "object",
            |  "additionalProperties" : {
            |    "type" : "integer"
            |  }
            |}""".stripMargin,
        render(json.editions) ==
          """{
            |  "$schema" : "https://json-schema.org/draft/2020-12/schema",
            |  "type" : "object",
            |  "additionalProperties" : {
            |    "type" : "integer"
            |  },
            |  "propertyNames" : {
            |    "type" : "string",
            |    "format" : "uuid"
            |  }
            |}""".stripMargin
      )
    ,
    test("a dictionary carried as a list of pairs is an object too, because that is what it decodes from"):
      assertTrue(render(json.printings) == render(dictionary.map(json.counter, string)))
  )
