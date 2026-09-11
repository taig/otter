package io.taig.otter.http.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Keys
import io.taig.otter.http.Endpoint
import io.taig.otter.http.MediaType
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiDocument
import io.taig.otter.http.OpenApiIssue
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import zio.Scope
import zio.test.*

object OpenApiResponseAlternativesTest extends ZIOSpecDefault:
  private val renderer = OpenApiRenderer.server(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
  private val requestSchema = request(method.get, __ / "alternatives")

  private def render(value: Endpoint.Node): OpenApiDocument =
    renderer.render(OpenApi.Info("Alternatives", "1"), Chain.one(value))

  private def response(document: OpenApiDocument): CirceJson =
    document.value.hcursor
      .downField("paths")
      .downField("/alternatives")
      .downField("get")
      .downField("responses")
      .downField("200")
      .focus
      .getOrElse(CirceJson.Null)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiResponseAlternativesTest")(
    test("one status retains every media type and description"):
      val document = render(
        endpoint(
          requestSchema,
          result(code.ok).body(body.json(payload.int)).attr(Keys.description, "A number") :+
            result(code.ok).body(body(MediaType.Text, payload.string)).attr(Keys.description, "Text")
        )
      )
      val value = response(document)
      assertTrue(
        document.issues.isEmpty,
        value.hcursor.get[String]("description") == Right("A number\n\nText"),
        value.hcursor
          .downField("content")
          .downField("application/json")
          .downField("schema")
          .get[String]("type") == Right("integer"),
        value.hcursor.downField("content").downField("text/plain").downField("schema").get[String]("type") == Right(
          "string"
        )
      )
    ,
    test("one media type retains overlapping schema alternatives"):
      val document = render(
        endpoint(
          requestSchema,
          result(code.ok).body(body.json(payload.int)) :+ result(code.ok).body(body.json(payload.double))
        )
      )
      val schema = response(document).hcursor.downField("content").downField("application/json").downField("schema")
      assertTrue(
        document.issues.isEmpty,
        schema.get[List[CirceJson]]("anyOf") == Right(
          List(
            CirceJson.obj("type" -> CirceJson.fromString("integer")),
            CirceJson.obj("type" -> CirceJson.fromString("number"))
          )
        )
      )
    ,
    test("identical alternatives need no schema union"):
      val document = render(
        endpoint(
          requestSchema,
          result(code.ok).body(body.json(payload.int)) :+ result(code.ok).body(body.json(payload.int))
        )
      )
      val schema = response(document).hcursor.downField("content").downField("application/json").downField("schema")
      assertTrue(
        document.issues.isEmpty,
        schema.get[String]("type") == Right("integer"),
        schema.downField("anyOf").focus.isEmpty
      )
    ,
    test("different headers are optional unless every branch requires them and report lost correlations"):
      val document = render(
        endpoint(
          requestSchema,
          result(code.ok).headers(header("X-Count", int).toRecord).body(body.json(payload.int)) :+
            result(code.ok).body(body.json(payload.string))
        )
      )
      val headers = response(document).hcursor.downField("headers")
      assertTrue(
        document.issues.contains(OpenApiIssue.ResponseAlternatives("GET /alternatives", 200)),
        headers.downField("X-Count").get[Boolean]("required") == Right(false),
        headers.downField("X-Count").downField("schema").get[String]("type") == Right("integer")
      )
    ,
    test("case insensitive header alternatives share one header and retain both schemas"):
      val document = render(
        endpoint(
          requestSchema,
          result(code.ok).headers(header("X-Value", int).toRecord).body(body.json(payload.int)) :+
            result(code.ok).headers(header("x-value", string).toRecord).body(body.json(payload.string))
        )
      )
      val headers = response(document).hcursor.downField("headers")
      assertTrue(
        document.issues.contains(OpenApiIssue.ResponseAlternatives("GET /alternatives", 200)),
        headers.focus.flatMap(_.asObject).map(_.keys.toList) == Some(List("X-Value")),
        headers.downField("X-Value").get[Boolean]("required") == Right(true),
        headers.downField("X-Value").downField("schema").get[List[CirceJson]]("anyOf").map(_.size) == Right(2)
      )
    ,
    test("an empty response alternative is reported without dropping the body"):
      val document = render(endpoint(requestSchema, result(code.ok) :+ result(code.ok).body(body.json(payload.int))))
      assertTrue(
        document.issues.contains(OpenApiIssue.ResponseAlternatives("GET /alternatives", 200)),
        response(document).hcursor.downField("content").downField("application/json").focus.isDefined
      )
    ,
    test("body alternatives within one result retain every schema"):
      val document =
        render(
          endpoint(requestSchema, result(code.ok).bodies(body.json(payload.int) :+ body.json(payload.string)).toUnion)
        )
      val schema = response(document).hcursor.downField("content").downField("application/json").downField("schema")
      assertTrue(document.issues.isEmpty, schema.get[List[CirceJson]]("anyOf").map(_.size) == Right(2))
    ,
    test("identical required headers remain required without a correlation issue"):
      val common = header("X-Count", int).toRecord
      val document = render(
        endpoint(
          requestSchema,
          result(code.ok).headers(common).body(body.json(payload.int)) :+
            result(code.ok).headers(common).body(body.json(payload.string))
        )
      )
      assertTrue(
        document.issues.isEmpty,
        response(document).hcursor.downField("headers").downField("X-Count").get[Boolean]("required") == Right(true)
      )
  )
