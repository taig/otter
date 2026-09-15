package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Keys
import io.taig.otter.http.Api
import io.taig.otter.http.Endpoint
import io.taig.otter.http.ErrorOverrides
import io.taig.otter.http.ErrorPolicy
import io.taig.otter.http.Failure
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiDocument
import io.taig.otter.http.OpenApiIssue
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.Status
import io.taig.otter.http.fixture.dsl
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import zio.Scope
import zio.test.*

object OpenApiResponseAlternativesTest extends ZIOSpecDefault:
  /** Rendering must inspect the declaration without evaluating this deliberately failing mapping. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  private def unexpectedMapping(failure: Failure): String = throw new IllegalStateException(failure.category.toString)

  private val renderer = OpenApiRenderer.server(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
  private val requestSchema = request(method.get, __ / "alternatives")

  private def render(value: Endpoint.Node): OpenApiDocument =
    renderer.render(OpenApi.Info("Alternatives", "1"), Chain.one(value))

  private def answer(document: OpenApiDocument): CirceJson =
    document.value.hcursor
      .downField("paths")
      .downField("/alternatives")
      .downField("get")
      .downField("responses")
      .downField("200")
      .focus
      .getOrElse(CirceJson.Null)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiResponseAlternativesTest")(
    test("composed errors are rendered from declarations without executing mappings"):
      val error = response(Status(503))(body.json(payload.string))
        .dimap[Failure, String](OpenApiResponseAlternativesTest.unexpectedMapping)(identity)
      val composed = ErrorPolicy.default.copy(unexpected = error)(endpoint(requestSchema, response(status.noContent)))
      val document = render(composed.effective)
      val responses =
        document.value.hcursor.downField("paths").downField("/alternatives").downField("get").downField("responses")
      assertTrue(
        document.issues.isEmpty,
        responses.keys.map(_.toSet).contains(Set("204", "400", "415", "422", "500", "503")),
        responses
          .downField("503")
          .downField("content")
          .downField("application/json")
          .downField("schema")
          .get[String]("type") == Right("string"),
        responses.downField("400").downField("content").focus.isEmpty
      )
    ,
    test("one status retains every media type and description"):
      val document = render(
        endpoint(
          requestSchema,
          response(status.ok)(body.json(payload.int)).attr(Keys.description, "A number") :+
            response(status.ok)(body(mediaType.text, payload.string)).attr(Keys.description, "Text")
        )
      )
      val value = answer(document)
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
          response(status.ok)(body.json(payload.int)) :+ response(status.ok)(body.json(payload.double))
        )
      )
      val schema = answer(document).hcursor.downField("content").downField("application/json").downField("schema")
      assertTrue(
        document.issues.isEmpty,
        schema.get[List[CirceJson]]("anyOf") == Right(
          List(
            CirceJson.obj(
              "type" -> CirceJson.fromString("integer"),
              "minimum" -> CirceJson.fromLong(-2147483648L),
              "maximum" -> CirceJson.fromLong(2147483647L)
            ),
            CirceJson.obj("type" -> CirceJson.fromString("number"))
          )
        )
      )
    ,
    test("identical alternatives need no schema union"):
      val document = render(
        endpoint(
          requestSchema,
          response(status.ok)(body.json(payload.int)) :+ response(status.ok)(body.json(payload.int))
        )
      )
      val schema = answer(document).hcursor.downField("content").downField("application/json").downField("schema")
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
          response(status.ok).headers(header("X-Count", int).toRecord)(body.json(payload.int)) :+
            response(status.ok)(body.json(payload.string))
        )
      )
      val headers = answer(document).hcursor.downField("headers")
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
          response(status.ok).headers(header("X-Value", int).toRecord)(body.json(payload.int)) :+
            response(status.ok).headers(header("x-value", string).toRecord)(body.json(payload.string))
        )
      )
      val headers = answer(document).hcursor.downField("headers")
      assertTrue(
        document.issues.contains(OpenApiIssue.ResponseAlternatives("GET /alternatives", 200)),
        headers.focus.flatMap(_.asObject).map(_.keys.toList) == Some(List("X-Value")),
        headers.downField("X-Value").get[Boolean]("required") == Right(true),
        headers.downField("X-Value").downField("schema").get[List[CirceJson]]("anyOf").map(_.size) == Right(2)
      )
    ,
    test("an empty response alternative is reported without dropping the body"):
      val document = render(endpoint(requestSchema, response(status.ok) :+ response(status.ok)(body.json(payload.int))))
      assertTrue(
        document.issues.contains(OpenApiIssue.ResponseAlternatives("GET /alternatives", 200)),
        answer(document).hcursor.downField("content").downField("application/json").focus.isDefined
      )
    ,
    test("body alternatives within one response retain every schema"):
      val document =
        render(
          endpoint(requestSchema, response(status.ok)(body.json(payload.int) :+ body.json(payload.string)).toUnion)
        )
      val schema = answer(document).hcursor.downField("content").downField("application/json").downField("schema")
      assertTrue(document.issues.isEmpty, schema.get[List[CirceJson]]("anyOf").map(_.size) == Right(2))
    ,
    test("identical required headers remain required without a correlation issue"):
      val common = header("X-Count", int).toRecord
      val document = render(
        endpoint(
          requestSchema,
          response(status.ok).headers(common)(body.json(payload.int)) :+
            response(status.ok).headers(common)(body.json(payload.string))
        )
      )
      assertTrue(
        document.issues.isEmpty,
        answer(document).hcursor.downField("headers").downField("X-Count").get[Boolean]("required") == Right(true)
      )
    ,
    test("standalone endpoint overrides inherit the default error policy"):
      val error = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "unavailable")(identity)
      val declared = endpoint(requestSchema, response(status.noContent))
        .withErrors(ErrorOverrides(unexpected = Some(error)))
      val document = renderer.render(OpenApi.Info("Standalone", "1"), Chain.one(declared))
      val expected = renderer.render(OpenApi.Info("Standalone", "1"), Api(ErrorPolicy.default, declared))
      assertTrue(document == expected)
    ,
    test("an API inherits defaults and replaces only the categories it overrides"):
      val first: Endpoint.Server[dsl.Payload, Unit, Unit] =
        endpoint(request(method.get, __ / "api-first"), response(status.ok).toUnion)
      val second: Endpoint.Server[dsl.Payload, Unit, Unit] =
        endpoint(request(method.get, __ / "api-second"), response(status.ok).toUnion)
      val defaults: ErrorPolicy[dsl.Payload, Status] = ErrorPolicy.default
      val syntax = response(Status(400))(body.json(payload.string)).dimap[Failure, Status](_ => "")(_ => Status(400))
      val api = Api(defaults, first, second.withErrors(ErrorOverrides(syntax = Some(syntax))))
      val document = renderer.render(OpenApi.Info("Alternatives", "1"), api)
      val paths = document.value.hcursor.downField("paths")
      val firstResponse = paths.downField("/api-first").downField("get").downField("responses").downField("400")
      val secondResponse = paths.downField("/api-second").downField("get").downField("responses").downField("400")
      assertTrue(
        document.issues == List(OpenApiIssue.ResponseAlternatives("GET /api-second", 400)),
        firstResponse.downField("content").focus.isEmpty,
        secondResponse
          .downField("content")
          .downField("application/json")
          .downField("schema")
          .get[String]("type") == Right("string")
      )
    ,
    test("duplicate operations are reported by the renderer without rejecting the API"):
      val registered = endpoint(request(method.get, __ / "registered"), response(status.ok).toUnion)
      val api = Api(ErrorPolicy.default, registered, registered)
      val document = renderer.render(OpenApi.Info("Duplicates", "1"), api)
      assertTrue(document.issues.contains(OpenApiIssue.Duplicate("get /registered")))
    ,
    test("complete responses shared by operations become deterministic response components"):
      val first = endpoint(request(method.get, __ / "first"), response(status.ok)(body.json(payload.string)).toUnion)
      val second = endpoint(request(method.get, __ / "second"), response(status.ok)(body.json(payload.string)).toUnion)
      val third = endpoint(request(method.get, __ / "third"), response(status.ok)(body.json(payload.int)).toUnion)
      val fourth = endpoint(request(method.get, __ / "fourth"), response(status.ok)(body.json(payload.int)).toUnion)

      val document = renderer.render(
        OpenApi.Info("Alternatives", "1"),
        Chain(first, second, third, fourth)
      )
      val responses = document.value.hcursor.downField("components").downField("responses")
      val reference = (path: String) =>
        document.value.hcursor
          .downField("paths")
          .downField(path)
          .downField("get")
          .downField("responses")
          .downField("200")
          .get[String]("$ref")

      assertTrue(
        document.issues.isEmpty,
        responses.focus.flatMap(_.asObject).map(_.keys.toList) == Some(List("Response200", "Response200_2")),
        reference("/first") == Right("#/components/responses/Response200"),
        reference("/third") == Right("#/components/responses/Response200_2")
      )
  )
