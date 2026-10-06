package io.taig.otter.codec

import io.taig.otter.JsonDiscriminator
import io.taig.otter.Side
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.TaggedTree
import zio.Scope
import zio.test.*

object JsonTypescriptDiscriminatorTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptDiscriminatorTest")(
    test("nested branches are structs with literal tags, including a singleton without a value"):
      val schema = branch.nested("code", string) :+ branch.nested("none", TNil)
      val source = JsonTypescriptEffectRenderer.writer.render(schema).mkString("\n\n")
      assertTrue(source == """Schema.Union(
                             |  [
                             |    Schema.Struct({
                             |      "type": Schema.Literal("code"),
                             |      "value": Schema.String
                             |    }),
                             |    Schema.Struct({ "type": Schema.Literal("none") })
                             |  ]
                             |)""".stripMargin)
    ,
    test("merged branches put the custom literal tag beside their fields"):
      val schema = branch.merged("code", field("text", string).toRecord, JsonDiscriminator.Merged("kind")) :+
        branch.merged("none", RNil, JsonDiscriminator.Merged("kind"))
      val source = JsonTypescriptEffectRenderer.reader.render(schema).mkString("\n\n")
      assertTrue(source == """Schema.Union(
                             |  [
                             |    Schema.Struct({
                             |      "kind": Schema.Literal("code"),
                             |      "text": Schema.String
                             |    }),
                             |    Schema.Struct({ "kind": Schema.Literal("none") })
                             |  ]
                             |)""".stripMargin)
    ,
    test("plain TypeScript narrows on the literal tag"):
      val schema = branch.nested("code", string) :+ branch.nested("none", TNil)
      val source = JsonTypescriptTypeRenderer(Side.Write).render(schema).render
      assertTrue(source == """|| {
                              |    "type": "code";
                              |    "value": string;
                              |  }
                              || { "type": "none" }""".stripMargin)
    ,
    test("recursive tagged unions keep literal tags in the explicit fixpoint type"):
      val source = JsonTypescriptEffectRenderer.writer.render(TaggedTree.schema).mkString("\n\n")
      assertTrue(
        source.contains("export type TaggedTree ="),
        source.contains("\"type\": \"fork\""),
        source.contains("ReadonlyArray<TaggedTree>"),
        source.contains("Schema.suspend(() => TaggedTree)"),
        source.contains("Schema.Literal(\"end\")")
      )
  )
