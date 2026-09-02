package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.Comparison
import io.taig.validation.std
import zio.Scope
import zio.test.*

import java.util.regex.Pattern

/** What a `Validation` says, said as keywords.
  *
  * A validation is introspectable, so everything a primitive was built with is still there to read back. What has no
  * counterpart is dropped rather than approximated: a document that constrains less than the decoder is safe, one that
  * constrains something else is not.
  */
object JsonSchemaConstraintTest extends ZIOSpecDefault:
  private def keywords(schema: Json.Node[?, ?]): Map[String, CirceJson] =
    JsonSchemaRenderer
      .writer(JsonSchemaProfile.Draft202012)
      .render(schema)
      .value
      .asObject
      .map(_.toMap.removed("$schema").removed("type"))
      .getOrElse(Map.empty)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaConstraintTest")(
    test("a length is a length"):
      assertTrue(
        keywords(string(std.text.minimum(1))) == Map("minLength" -> CirceJson.fromInt(1)),
        keywords(string(std.text.maximum(64))) == Map("maxLength" -> CirceJson.fromInt(64))
      )
    ,
    test("an exclusive bound on a length is the inclusive one next to it, because a length is an integer"):
      assertTrue(
        keywords(string(std.text.minimum(Comparison(1L, exclusive = true)))) ==
          Map("minLength" -> CirceJson.fromInt(2)),
        keywords(string(std.text.maximum(Comparison(64L, exclusive = true)))) ==
          Map("maxLength" -> CirceJson.fromInt(63))
      )
    ,
    test("a pattern is anchored, because matches is a whole match and pattern is not"):
      assertTrue(
        keywords(string(std.text.matches(Pattern.compile("[a-z]+")))) ==
          Map("pattern" -> CirceJson.fromString("^(?:[a-z]+)$")),
        keywords(string(std.text.matches(Pattern.compile("a|b")))) ==
          Map("pattern" -> CirceJson.fromString("^(?:a|b)$"))
      )
    ,
    test("a numeric bound keeps its strictness rather than being moved, because a number is not an integer"):
      assertTrue(
        keywords(int(std.number.minimum(Comparison(0, exclusive = false)))) ==
          Map("minimum" -> CirceJson.fromInt(0)),
        keywords(int(std.number.minimum(Comparison(0, exclusive = true)))) ==
          Map("exclusiveMinimum" -> CirceJson.fromInt(0)),
        keywords(int(std.number.maximum(Comparison(10, exclusive = false)))) ==
          Map("maximum" -> CirceJson.fromInt(10)),
        keywords(int(std.number.maximum(Comparison(10, exclusive = true)))) ==
          Map("exclusiveMaximum" -> CirceJson.fromInt(10)),
        keywords(int(std.number.multiple(2))) == Map("multipleOf" -> CirceJson.fromInt(2))
      )
    ,
    test("a collection's size and uniqueness carry, and its order does not"):
      assertTrue(
        keywords(collection.list(int, std.collection.minimum(1))) ==
          Map("items" -> CirceJson.obj("type" -> CirceJson.fromString("integer")), "minItems" -> CirceJson.fromInt(1)),
        keywords(collection.list(int, std.collection.uniqueItemsF[List, Int])) ==
          Map("items" -> CirceJson.obj("type" -> CirceJson.fromString("integer")), "uniqueItems" -> CirceJson.True)
      )
    ,
    test("a dictionary's size is a property count"):
      assertTrue(
        keywords(dictionary.map(string, int, std.obj.minimum(1))).get("minProperties") ==
          Some(CirceJson.fromInt(1))
      )
  )
