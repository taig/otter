package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Side
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

object JsonSchemaAbsenceTest extends ZIOSpecDefault:
  private def render(side: Side, field: Json.Field.Node[?, ?]): CirceJson =
    JsonSchemaRenderer(side, JsonSchemaProfile.Draft202012).render(field.toRecord).value

  private def required(document: CirceJson): Boolean =
    document.hcursor.get[List[String]]("required").toOption.exists(_.contains("x"))

  private def nullable(document: CirceJson): Boolean =
    document.hcursor.downField("properties").downField("x").focus.exists(_.noSpaces.contains("\"type\":\"null\""))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaAbsenceTest")(
    suite("read/write presence")(
      List[(String, Json.Field.Node[?, ?], Boolean, Boolean, Boolean, Boolean)](
        ("optional", field("x", int).optional, false, false, false, false),
        ("nullable", field("x", int).nullable, true, true, true, true),
        ("optionalOrNull", field("x", int).optionalOrNull, false, true, false, false),
        ("nullableOrMissing", field("x", int).nullableOrMissing, false, true, true, true),
        ("defaulted", field("x", int).defaulted(7), false, false, true, false),
        ("defaultedOnNull", field("x", int).defaultedOnNull(7), true, true, true, false),
        ("defaultedOnMissingOrNull", field("x", int).defaultedOnMissingOrNull(7), false, true, true, false),
        ("PATCH", field("x", int.nullable).optional, false, true, false, true)
      ).map { (name, field, readRequired, readNull, writeRequired, writeNull) =>
        test(name):
          val read = render(Side.Read, field)
          val write = render(Side.Write, field)
          assertTrue(
            required(read) == readRequired,
            nullable(read) == readNull,
            required(write) == writeRequired,
            nullable(write) == writeNull
          )
      }
    ),
    test("runtime defaults do not invent annotations; annotations do not change runtime defaults"):
      val plain = field("x", int).defaulted(7)
      val documented = plain.attr(JsonSchemaKeys.default, CirceJson.fromInt(8))
      val document = render(Side.Read, documented)
      assertTrue(
        render(Side.Read, plain).hcursor.downField("properties").downField("x").downField("default").focus.isEmpty,
        document.hcursor.downField("properties").downField("x").get[Int]("default").toOption.contains(8),
        JsonCirceDecoder.decode(documented.toRecord, CirceJson.obj()).toOption.contains(7)
      )
    ,
    test("strict profiles still report omission-only contracts they cannot faithfully represent"):
      val strict = JsonSchemaRenderer.reader(JsonSchemaProfile.Strict)
      assertTrue(
        strict.render(field("x", boolean).optional.toRecord).issues.nonEmpty,
        strict.render(field("x", boolean).optionalOrNull.toRecord).issues.isEmpty
      )
  )
