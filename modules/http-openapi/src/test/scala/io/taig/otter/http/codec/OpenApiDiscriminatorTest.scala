package io.taig.otter.http.codec

import io.taig.otter.JsonDiscriminator
import io.taig.otter.Keys
import io.taig.otter.Side
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.http.OpenApiProfile
import zio.Scope
import zio.test.*

object OpenApiDiscriminatorTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiDiscriminatorTest")(
    test("named merged branches have discriminator mappings to their tagged definitions"):
      val schema = branch.merged("circle", field("radius", int).toRecord.attr(Keys.name, "Circle")) :+
        branch.merged("none", RNil.attr(Keys.name, "None"))
      val document = OpenApiPayload.json(OpenApiProfile.V31).render(Side.Write, schema)
      val root = document.map(_.value.hcursor)
      assertTrue(
        document.exists(_.issues.isEmpty),
        root.map(_.downField("discriminator").get[String]("propertyName")) == Some(Right("type")),
        root.map(_.downField("discriminator").downField("mapping").get[String]("circle")) == Some(
          Right("#/components/schemas/Circle")
        ),
        root.map(
          _.downField("components/schemas")
            .downField("Circle")
            .downField("properties")
            .downField("type")
            .get[String]("const")
        ) == Some(Right("circle"))
      )
    ,
    test("inline nested branches use their custom property without inventing reference mappings"):
      val schema = branch.nested("code", string, JsonDiscriminator.Nested("kind", "data")) :+
        branch.nested("none", TNil, JsonDiscriminator.Nested("kind", "data"))
      val document = OpenApiPayload.json(OpenApiProfile.V31).render(Side.Read, schema)
      assertTrue(
        document.map(_.value.hcursor.downField("discriminator").get[String]("propertyName")) == Some(Right("kind")),
        document.flatMap(_.value.hcursor.downField("discriminator").downField("mapping").focus).isEmpty,
        document.flatMap(_.value.hcursor.downField("oneOf").focus).isDefined
      )
  )
