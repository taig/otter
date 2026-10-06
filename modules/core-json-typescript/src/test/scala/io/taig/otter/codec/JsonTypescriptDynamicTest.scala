package io.taig.otter.codec

import io.taig.otter.Side
import io.taig.otter.component.JsonComponent.*
import zio.Scope
import zio.test.*

object JsonTypescriptDynamicTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonTypescriptDynamicTest")(
    test("dynamic variants use open TypeScript types on both sides"):
      val renderer = JsonTypescriptTypeRenderer(Side.Read)
      assertTrue(
        renderer.render(dynamic.any).render == "unknown",
        renderer.render(dynamic.obj).render == "Record<string, unknown>",
        renderer.render(dynamic.array).render == "ReadonlyArray<unknown>"
      )
  )
