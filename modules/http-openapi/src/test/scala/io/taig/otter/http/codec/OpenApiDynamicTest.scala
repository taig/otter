package io.taig.otter.http.codec

import io.taig.otter.Side
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.http.OpenApiProfile
import zio.Scope
import zio.test.*

object OpenApiDynamicTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiDynamicTest")(
    test("JSON payload schemas render arbitrary JSON values, objects, and arrays"):
      val payload = OpenApiPayload.json(OpenApiProfile.V31)

      assertTrue(
        payload.render(Side.Write, dynamic.any).map(_.value.noSpaces) == Some("{}"),
        payload.render(Side.Write, dynamic.obj).map(_.value.noSpaces) == Some("""{"type":"object"}"""),
        payload.render(Side.Write, dynamic.array).map(_.value.noSpaces) ==
          Some("""{"type":"array","items":{}}""")
      )
  )
