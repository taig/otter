package io.taig.otter.http.codec

import cats.data.NonEmptyList
import io.circe.Json as CirceJson
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.Metadata
import io.taig.otter.http.fixture.dsl.*
import io.taig.validation.Comparison
import io.taig.validation.std
import zio.Scope
import zio.test.*

object OpenApiParameterFidelityTest extends ZIOSpecDefault:
  private val renderer =
    OpenApiParameterRenderer(JsonSchemaProfile.Draft202012, NonEmptyList.one(Metadata.Namespace.Global))

  override val spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiParameterFidelityTest")(
    test("parameter bounds retain their carrier and repeated constraints"):
      val document = renderer.render(
        int(std.number.minimum[Int](Comparison(10, false)) & std.number.minimum[Int](Comparison(0, false)))
      )
      val bounds = List(-2147483648, 10, 0).map(value => CirceJson.obj("minimum" -> CirceJson.fromInt(value)))
      assertTrue(
        document.value.hcursor.get[List[CirceJson]]("allOf").contains(bounds),
        document.value.hcursor.get[Long]("maximum").contains(2147483647L),
        document.issues.isEmpty
      )
    ,
    test("parameter text lengths report the same loss as payload lengths"):
      val document = renderer.render(string(std.text.minimum[String](2)))
      assertTrue(document.value.noSpaces == """{"type":"string"}""", document.issues.size == 1)
  )
