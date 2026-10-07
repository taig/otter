package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.NonEmptySet
import io.circe.Json as CirceJson
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import io.taig.validation.std
import zio.Scope
import zio.test.*

object OpenApiCollectionTest extends ZIOSpecDefault:
  private val items = payload.collection.nonEmptySet(payload.string, std.collection.maximum[NonEmptySet[String]](10))
  private val declaration = endpoint(
    request(method.post, __)
      .queries(query("ids", collection.nonEmptySet(string)).toRecord)
      .headers(header("X-Ids", collection.nonEmptyList(string)).toRecord)(body.json(items)),
    response(status.ok)(body.json(items))
  )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiCollectionTest")(
    test("server and client documents carry collection constraints in bodies, queries and headers"):
      val renderers = List(
        OpenApiRenderer.server(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31)),
        OpenApiRenderer.client(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
      )
      assertTrue(renderers.forall { renderer =>
        val document = renderer.render(OpenApi.Info("Collections", "1"), Chain(declaration))
        val operation = document.value.hcursor.downField("paths").downField("/").downField("post")
        val input =
          operation.downField("requestBody").downField("content").downField("application/json").downField("schema")
        val output = operation
          .downField("responses")
          .downField("200")
          .downField("content")
          .downField("application/json")
          .downField("schema")
        val parameters = operation.get[List[CirceJson]]("parameters").getOrElse(Nil)
        document.issues.isEmpty && List(input, output).forall(cursor =>
          cursor.get[Long]("minItems").contains(1L) && cursor.get[Long]("maxItems").contains(10L) &&
            cursor.get[Boolean]("uniqueItems").contains(true)
        ) && parameters.size == 2 && parameters.forall(
          _.hcursor.downField("schema").get[Long]("minItems").contains(1L)
        ) &&
        parameters
          .find(_.hcursor.get[String]("name").contains("ids"))
          .exists(_.hcursor.downField("schema").get[Boolean]("uniqueItems").contains(true))
      })
  )
