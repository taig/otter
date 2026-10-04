package io.taig.otter.http.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.JsonSchemaKeys
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.fixture.dsl.*
import zio.Scope
import zio.test.*

object OpenApiAbsenceTest extends ZIOSpecDefault:
  private val server = OpenApiRenderer.server(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
  private val client = OpenApiRenderer.client(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
  private val declaration = endpoint(
    request(method.get, __)
      .queries(
        query("page", int).defaulted(7).attr(JsonSchemaKeys.default, CirceJson.fromInt(7)) :*
          query("empty", int).empty :* query("lenient", int).emptyOrMissing :*
          query("onEmpty", int).defaultedOnEmpty(9) :* query.flag("flag")
      )
      .headers(header("X-Limit", int).defaulted(4).toRecord),
    response(status.ok).headers(
      header("X-Count", int).defaulted(5).attr(JsonSchemaKeys.default, CirceJson.fromInt(5)).toRecord
    )
  )

  private def operation(renderer: OpenApiRenderer): CirceJson =
    renderer
      .render(OpenApi.Info("Absence", "1"), Chain(declaration))
      .value
      .hcursor
      .downField("paths")
      .downField("/")
      .downField("get")
      .focus
      .getOrElse(CirceJson.Null)

  private def parameters(document: CirceJson): Map[String, CirceJson] =
    document.hcursor
      .get[List[CirceJson]]("parameters")
      .getOrElse(Nil)
      .flatMap(value => value.hcursor.get[String]("name").toOption.map(_ -> value))
      .toMap

  private def required(parameters: Map[String, CirceJson], name: String): Option[Boolean] =
    parameters.get(name).flatMap(_.hcursor.get[Boolean]("required").toOption)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiAbsenceTest")(
    test("query and request-header requiredness follows the selected side"):
      val reading = parameters(operation(server))
      val writing = parameters(operation(client))
      assertTrue(
        required(reading, "page").contains(false),
        required(writing, "page").contains(true),
        required(reading, "empty").contains(true),
        required(writing, "empty").contains(true),
        required(reading, "lenient").contains(false),
        required(writing, "lenient").contains(true),
        required(reading, "onEmpty").contains(true),
        required(writing, "onEmpty").contains(true),
        required(reading, "flag").contains(false),
        required(writing, "flag").contains(true),
        required(reading, "X-Limit").contains(false),
        required(writing, "X-Limit").contains(true),
        reading.get("page").flatMap(_.hcursor.downField("schema").get[Int]("default").toOption).contains(7),
        reading.get("flag").flatMap(_.hcursor.downField("schema").downField("default").focus).isEmpty
      )
    ,
    test("response header defaults and annotations survive in the opposite direction"):
      val writing =
        operation(server).hcursor.downField("responses").downField("200").downField("headers").downField("X-Count")
      val reading =
        operation(client).hcursor.downField("responses").downField("200").downField("headers").downField("X-Count")
      assertTrue(
        writing.get[Boolean]("required").toOption.contains(true),
        reading.get[Boolean]("required").toOption.contains(false),
        writing.downField("schema").get[Int]("default").toOption.contains(5)
      )
  )
