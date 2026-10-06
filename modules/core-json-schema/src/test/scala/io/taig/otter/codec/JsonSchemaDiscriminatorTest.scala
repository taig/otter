package io.taig.otter.codec

import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Side
import io.taig.otter.fixture.Tagged
import zio.Scope
import zio.test.*

object JsonSchemaDiscriminatorTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonSchemaDiscriminatorTest")(
    test("nested branches are disjoint objects, with no payload for the singleton"):
      val document = JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012).render(Tagged.nested)
      val branches = document.value.hcursor.downField("oneOf")
      val circle = branches.downArray
      val none = branches.downN(3)
      assertTrue(
        document.issues.isEmpty,
        circle.downField("properties").downField("type").get[String]("const") == Right("circle"),
        circle.get[List[String]]("required") == Right(List("type", "value")),
        circle
          .downField("properties")
          .downField("value")
          .downField("properties")
          .downField("radius")
          .get[String]("type") == Right("integer"),
        none.get[List[String]]("required") == Right(List("type")),
        none.downField("properties").downField("value").focus.isEmpty,
        document.value.hcursor.downField("discriminator").focus.isEmpty
      )
    ,
    test("merged records contain their required tag on both sides"):
      val results = List(Side.Read, Side.Write).map: side =>
        val document = JsonSchemaRenderer(side, JsonSchemaProfile.Draft202012).render(Tagged.merged)
        val circle = document.value.hcursor.downField("oneOf").downArray
        document.issues.isEmpty &&
        circle.get[List[String]]("required") == Right(List("type", "radius")) &&
        circle.downField("properties").downField("type").get[String]("const") == Right("circle")
      assertTrue(results.forall(identity))
    ,
    test("custom keys describe the actual wire shape"):
      val code = JsonSchemaRenderer
        .writer(JsonSchemaProfile.Draft202012)
        .render(Tagged.custom)
        .value
        .hcursor
        .downField("oneOf")
        .downArray
      assertTrue(
        code.get[List[String]]("required") == Right(List("kind", "data")),
        code.downField("properties").downField("kind").get[String]("const") == Right("code")
      )
  )
