package io.taig.otter.http.codec

import cats.data.Chain
import io.circe.ACursor
import io.circe.Json as CirceJson
import io.taig.otter.http.Code
import io.taig.otter.http.MediaType
import io.taig.otter.http.Method
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiDocument
import io.taig.otter.http.OpenApiIssue
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.fixture.*
import io.taig.otter.http.fixture.dsl.*
import zio.Scope
import zio.test.*

/** What an OpenAPI document says about the endpoints it came from.
  *
  * Asserted a claim at a time rather than against one golden blob, so that a failure says which claim broke. The two
  * whole subtrees that are compared -- a multipart body and a set of parameters -- are the two places where the shape
  * as a whole is the point.
  */
object OpenApiRendererTest extends ZIOSpecDefault:
  private val server = OpenApiRenderer.server(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))

  private val client = OpenApiRenderer.client(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))

  private val info = OpenApi.Info("Reports", "1.0")

  private val document = server.render(info, Chain(api.fetch, api.replace, api.stream))

  extension (self: OpenApiDocument)
    private def at(path: String*): Option[CirceJson] =
      path.foldLeft(self.value.hcursor: ACursor)(_.downField(_)).focus

  /** A payload alphabet nothing has a renderer for, which is what an unrecognised body looks like from the inside. */
  final private case class Unknown[-W, +R]()

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiRendererTest")(
    suite("document")(
      test("declares the version it was written for and what it describes"):
        assertTrue(document.at("openapi").flatMap(_.asString) == Some(OpenApi.Version)) &&
        assertTrue(document.at("info", "title").flatMap(_.asString) == Some("Reports")) &&
        assertTrue(document.at("info", "version").flatMap(_.asString) == Some("1.0"))
      ,
      test("files each operation under its path template and its method"):
        assertTrue(
          document.at("paths").flatMap(_.asObject).map(_.keys.toList) == Some(List("/reports/{id}", "/reports"))
        )
        &&
        assertTrue(
          document.at("paths", "/reports/{id}").flatMap(_.asObject).map(_.keys.toList) == Some(List("get", "put"))
        )
      ,
      test("names an operation after its method and path when the schema says nothing"):
        assertTrue(
          document.at("paths", "/reports/{id}", "get", "operationId").flatMap(_.asString) ==
            Some("GET /reports/{id}")
        )
    ),
    suite("parameters")(
      test("a path placeholder is required and typed by what stands in it"):
        val expected = CirceJson.arr(
          CirceJson.obj(
            "name" -> CirceJson.fromString("id"),
            "in" -> CirceJson.fromString("path"),
            "required" -> CirceJson.True,
            "schema" -> CirceJson.obj("type" -> CirceJson.fromString("integer"))
          ),
          CirceJson.obj(
            "name" -> CirceJson.fromString("page"),
            "in" -> CirceJson.fromString("query"),
            "required" -> CirceJson.False,
            "schema" -> CirceJson.obj("type" -> CirceJson.fromString("integer"))
          )
        )

        assertTrue(document.at("paths", "/reports/{id}", "get", "parameters") == Some(expected))
      ,
      test("an operation with nothing to ask for lists no parameters"):
        assertTrue(document.at("paths", "/reports", "get", "parameters").isEmpty)
    ),
    suite("responses")(
      test("carries the phrase its own code gives, since a description is not optional"):
        assertTrue(
          document.at("paths", "/reports/{id}", "get", "responses", "200", "description").flatMap(_.asString) ==
            Some("OK")
        ) &&
        assertTrue(
          document.at("paths", "/reports/{id}", "get", "responses", "404", "description").flatMap(_.asString) ==
            Some("Not Found")
        )
      ,
      test("an answer with no entity lists no content"):
        assertTrue(document.at("paths", "/reports/{id}", "get", "responses", "404", "content").isEmpty)
    ),
    suite("components")(
      test("a named payload is declared once and referred to from everywhere it is used"):
        val reference = CirceJson.obj("$ref" -> CirceJson.fromString("#/components/schemas/Report"))

        assertTrue(
          document.at("paths", "/reports/{id}", "put", "responses", "200", "content", "application/json", "schema") ==
            Some(reference)
        ) &&
        assertTrue(document.at("components", "schemas").flatMap(_.asObject).map(_.keys.toList) == Some(List("Report")))
      ,
      test("an unnamed payload is written where it stands"):
        assertTrue(
          document
            .at("paths", "/reports/{id}", "get", "responses", "200", "content", "application/json", "schema", "type")
            .flatMap(_.asString) == Some("object")
        )
      ,
      test("two different schemas asking for one name keeps the first and says so"):
        val conflicting = api.settings.attr(io.taig.otter.Keys.name, "Report")

        val rendered = server.render(
          info,
          Chain(
            api.replace,
            endpoint(request(Method.Post, PNil :* segment("other")).body(json(conflicting)), result(Code.Ok).toUnion)
          )
        )

        assertTrue(rendered.issues.contains(OpenApiIssue.Conflict("Report"))) &&
        assertTrue(
          rendered.at("components", "schemas", "Report", "properties").flatMap(_.asObject).map(_.keys.toList) ==
            Some(List("title", "pages"))
        )
    ),
    suite("multipart")(
      test("is an object of parts with a content type, and a filename, for each"):
        val expected = CirceJson.obj(
          "type" -> CirceJson.fromString("object"),
          "properties" -> CirceJson.obj(
            "report" -> CirceJson.obj("$ref" -> CirceJson.fromString("#/components/schemas/Report")),
            "attachment" -> CirceJson.obj(
              "type" -> CirceJson.fromString("string"),
              "contentMediaType" -> CirceJson.fromString("application/pdf")
            )
          ),
          "required" -> CirceJson.arr(CirceJson.fromString("report")),
          "encoding" -> CirceJson.obj(
            "report" -> CirceJson.obj("contentType" -> CirceJson.fromString("application/json")),
            "attachment" -> CirceJson.obj(
              "contentType" -> CirceJson.fromString("application/pdf"),
              "contentDisposition" -> CirceJson.fromString("""form-data; filename="report.pdf"""")
            )
          )
        )

        assertTrue(
          document.at("paths", "/reports/{id}", "put", "requestBody", "content", "multipart/form-data", "schema") ==
            Some(expected)
        )
      ,
      test("a request that declares a body requires one"):
        assertTrue(
          document.at("paths", "/reports/{id}", "put", "requestBody", "required").flatMap(_.asBoolean) == Some(true)
        )
    ),
    suite("streamed")(
      test("is keyed by its framing media type and described by one element"):
        assertTrue(
          document
            .at("paths", "/reports", "get", "responses", "200", "content", "application/x-ndjson", "schema", "type")
            .flatMap(_.asString) == Some("object")
        )
      ,
      /** The framing is the one thing OpenAPI has no vocabulary for, so it is reported rather than implied. */
      test("says that the framing went unsaid"):
        assertTrue(document.issues == List(OpenApiIssue.Framed("GET /reports", MediaType.NdJson.render)))
    ),
    suite("sides")(
      /** The claim the two `Side`s exist for. A field holding a default is absent when read and always written, so a
        * document for callers and a document for the server disagree about it -- and the same schema value produces
        * both.
        */
      test("a defaulted field is required of a writer and not of a reader"):
        def schema(document: OpenApiDocument, key: String*): Option[CirceJson] =
          document.at(
            List("paths", "/settings", "put", "requestBody", "content", "application/json", "schema") ++ key*
          )

        val read = server.render(info, Chain(api.configure))
        val written = client.render(info, Chain(api.configure))

        assertTrue(schema(read, "required") == Some(CirceJson.arr())) &&
        assertTrue(schema(written, "required") == Some(CirceJson.arr(CirceJson.fromString("theme")))) &&
        assertTrue(
          schema(read, "properties", "theme") == Some(
            CirceJson.obj(
              "anyOf" -> CirceJson.arr(
                CirceJson.obj("type" -> CirceJson.fromString("string")),
                CirceJson.obj("type" -> CirceJson.fromString("null"))
              )
            )
          )
        ) &&
        assertTrue(
          schema(written, "properties", "theme") == Some(CirceJson.obj("type" -> CirceJson.fromString("string")))
        )
    ),
    suite("recursion")(
      test("a payload that refers to itself is declared once and points at itself"):
        val rendered = server.render(info, Chain(api.trees))
        val reference = CirceJson.obj("$ref" -> CirceJson.fromString("#/components/schemas/Tree"))

        assertTrue(
          rendered.at("paths", "/trees", "get", "responses", "200", "content", "application/json", "schema") ==
            Some(reference)
        ) &&
        assertTrue(
          rendered.at("components", "schemas", "Tree", "properties", "children", "items") == Some(reference)
        ) &&
        assertTrue(rendered.issues.isEmpty)
    ),
    suite("agreement")(
      /** As `JsonSchemaAgreementTest` checks a rendered shape against the document circe writes, this checks a rendered
        * operation against what the envelope codecs actually accept. A document that named a placeholder the path
        * decoder does not read, or listed a parameter count the encoder does not produce, would describe an endpoint
        * nobody can call.
        */
      test("every path placeholder the document names is a segment the codec reads"):
        val template = document.at("paths").flatMap(_.asObject).map(_.keys.toList).getOrElse(Nil).head
        val placeholders = template.split("/").filter(_.startsWith("{")).map(_.stripPrefix("{").stripSuffix("}"))

        val named = document
          .at("paths", template, "get", "parameters")
          .flatMap(_.asArray)
          .getOrElse(Vector.empty)
          .filter(_.hcursor.downField("in").focus.flatMap(_.asString).contains("path"))
          .flatMap(_.hcursor.downField("name").focus.flatMap(_.asString))

        assertTrue(named == placeholders.toVector) &&
        assertTrue(template.split("/").length == PathEncoder.encode(api.one, 1).length + 1)
      ,
      test("every query parameter the document names is one the codec reads"):
        val named = document
          .at("paths", "/reports/{id}", "get", "parameters")
          .flatMap(_.asArray)
          .getOrElse(Vector.empty)
          .filter(_.hcursor.downField("in").focus.flatMap(_.asString).contains("query"))
          .flatMap(_.hcursor.downField("name").focus.flatMap(_.asString))

        assertTrue(named == QueriesEncoder.encode(api.paging, 2).map(_._1).toList.toVector)
    ),
    suite("shortfalls")(
      test("two operations on one method and path keep the first and say so"):
        val rendered = server.render(info, Chain(api.fetch, api.fetch))

        assertTrue(rendered.issues.contains(OpenApiIssue.Duplicate("get /reports/{id}"))) &&
        assertTrue(
          rendered.at("paths", "/reports/{id}").flatMap(_.asObject).map(_.keys.toList) == Some(List("get"))
        )
      ,
      test("a payload in an alphabet nothing describes is still listed, and reported"):
        val rendered = server.render(
          info,
          Chain(
            endpoint(
              request(Method.Post, PNil :* segment("opaque")).body(body(MediaType.Text, Unknown[String, String]())),
              result(Code.Ok).toUnion
            )
          )
        )

        assertTrue(rendered.issues == List(OpenApiIssue.Undescribed("POST /opaque", MediaType.Text.render))) &&
        assertTrue(
          rendered.at("paths", "/opaque", "post", "requestBody", "content", "text/plain", "schema") ==
            Some(CirceJson.obj())
        )
    )
  )
