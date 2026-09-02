package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** The two sides of a schema are two documents.
  *
  * Everywhere they agree there is nothing to say; this is the list of everywhere they do not, and it is why a
  * [[io.taig.otter.Side]] has to be handed to a renderer rather than recovered from what it is given.
  */
object JsonSchemaDirectionTest extends ZIOSpecDefault:
  private def read(schema: Json.Node[?, ?]): String =
    JsonSchemaRenderer.reader(JsonSchemaProfile.Draft202012).render(schema).value.noSpaces

  private def write(schema: Json.Node[?, ?]): String =
    JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012).render(schema).value.noSpaces

  private val Dialect = """"$schema":"https://json-schema.org/draft/2020-12/schema""""

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaDirectionTest")(
    test("a field that drops its key is optional when written and takes a null as well when read"):
      assertTrue(
        write(json.omittedTag) ==
          s"""{$Dialect,"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"type":"integer"}},"required":["title"]}""",
        read(json.omittedTag) ==
          s"""{$Dialect,"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"anyOf":[{"type":"integer"},{"type":"null"}]}},"required":["title"]}"""
      )
    ,
    test("a field that writes an explicit null is required on the way out"):
      assertTrue(
        write(json.nullableTag) ==
          s"""{$Dialect,"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"anyOf":[{"type":"integer"},{"type":"null"}]}},"required":["title","tag"]}""",
        read(json.nullableTag) ==
          s"""{$Dialect,"type":"object","properties":{"title":{"type":"string"},""" +
          """"tag":{"anyOf":[{"type":"integer"},{"type":"null"}]}},"required":["title"]}"""
      )
    ,
    test("a strict field that drops its key does not admit a null when read"):
      assertTrue(
        read(json.nestedTag) ==
          s"""{$Dialect,"type":"object","properties":""" +
          """{"tag":{"anyOf":[{"type":"integer"},{"type":"null"}]}},"required":[]}"""
      )
    ,
    test("an optional value is nullable on both sides, because it is a value and not a key"):
      assertTrue(
        write(field("bar", int.optional).toRecord) == read(field("bar", int.optional).toRecord),
        write(field("bar", int.optional).toRecord) ==
          s"""{$Dialect,"type":"object","properties":""" +
          """{"bar":{"anyOf":[{"type":"integer"},{"type":"null"}]}},"required":["bar"]}"""
      )
    ,
    test("a coercion says the laxer forms it takes only on the side that takes them"):
      assertTrue(
        write(coerce(int)) == s"""{$Dialect,"type":"integer"}""",
        read(coerce(int)) == s"""{$Dialect,"anyOf":[{"type":"integer"},{"type":"string"}]}""",
        read(coerce(string)) ==
          s"""{$Dialect,"anyOf":[{"type":"string"},{"type":"number"},{"type":"boolean"}]}""",
        read(coerce(boolean)) == s"""{$Dialect,"anyOf":[{"type":"boolean"},{"type":"string"}]}"""
      )
  )
