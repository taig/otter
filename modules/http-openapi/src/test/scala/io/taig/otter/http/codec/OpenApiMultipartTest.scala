package io.taig.otter.http.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Keys
import io.taig.otter.http.Endpoint
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiDocument
import io.taig.otter.http.OpenApiIssue
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import zio.Scope
import zio.test.*

object OpenApiMultipartTest extends ZIOSpecDefault:
  private val server = OpenApiRenderer.server(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
  private val client = OpenApiRenderer.client(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
  private val upload = request(method.post, __ / "upload")
  private val done = result(code.noContent).toUnion
  private val pdf = body.multipart(part("file", body.binary(mediaType.pdf)).filename("report.pdf").toRecord)

  private def render(value: Endpoint.Node, renderer: OpenApiRenderer = server): OpenApiDocument =
    renderer.render(OpenApi.Info("Multipart", "1"), Chain.one(value))

  private def content(document: OpenApiDocument, media: String = "multipart/form-data"): CirceJson =
    document.value.hcursor
      .downField("paths")
      .downField("/upload")
      .downField("post")
      .downField("requestBody")
      .downField("content")
      .downField(media)
      .focus
      .getOrElse(CirceJson.Null)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("OpenApiMultipartTest")(
    test("both sides keep body annotations on the schema and filename hints in a header example"):
      val endpointSchema = endpoint(upload(pdf.attr(Keys.description, "Upload a report")), done)
      val documents = List(render(endpointSchema), render(endpointSchema, client))
      assertTrue(documents.forall: document =>
        val value = content(document).hcursor
        val header = value.downField("encoding").downField("file").downField("headers").downField("Content-Disposition")
        document.issues.isEmpty &&
        value.downField("schema").get[String]("description").contains("Upload a report") &&
        value.downField("schema").downField("encoding").focus.isEmpty &&
        header.downField("schema").get[String]("type").contains("string") &&
        header.downField("schema").downField("const").focus.isEmpty &&
        header.get[String]("example").contains("""form-data; name="file"; filename="report.pdf"""") &&
        header.downField("required").focus.isEmpty)
    ,
    test("optional parts keep their encoding without becoming required"):
      val parts = part("file", body.binary(mediaType.pdf)).optional.toRecord
      val document = render(endpoint(upload(body.multipart(parts)), done))
      val value = content(document).hcursor
      assertTrue(
        document.issues.isEmpty,
        value.downField("schema").downField("required").focus.isEmpty,
        value.downField("encoding").downField("file").get[String]("contentType").contains("application/pdf"),
        value.downField("encoding").downField("file").downField("headers").focus.isEmpty
      )
    ,
    test("identical multipart alternatives retain one encoding map"):
      val document = render(endpoint(upload(pdf :+ pdf), done))
      assertTrue(
        document.issues.isEmpty,
        content(document).hcursor.downField("encoding").downField("file").focus.isDefined,
        content(document).hcursor.downField("schema").downField("anyOf").focus.isEmpty
      )
    ,
    test("different payload schemas can share an identical encoding"):
      val number = body.multipart(part("value", body.json(payload.int)).toRecord)
      val text = body.multipart(part("value", body.json(payload.string)).toRecord)
      val document = render(endpoint(upload(number :+ text), done))
      assertTrue(
        document.issues.isEmpty,
        content(document).hcursor.downField("schema").get[List[CirceJson]]("anyOf").map(_.size).contains(2),
        content(document).hcursor
          .downField("encoding")
          .downField("value")
          .get[String]("contentType")
          .contains("application/json")
      )
    ,
    test("conflicting part content types retain schemas and report omitted encoding"):
      val text = body.multipart(part("file", body.binary(mediaType.text)).toRecord)
      val document = render(endpoint(upload(pdf :+ text), done))
      assertTrue(
        document.issues == List(OpenApiIssue.Encoding("POST /upload", "multipart/form-data")),
        content(document).hcursor.downField("schema").get[List[CirceJson]]("anyOf").map(_.size).contains(2),
        content(document).hcursor.downField("encoding").focus.isEmpty
      )
    ,
    test("different filename hints do not silently choose the first alternative"):
      val other = body.multipart(part("file", body.binary(mediaType.pdf)).filename("other.pdf").toRecord)
      val document = render(endpoint(upload(pdf :+ other), done))
      assertTrue(
        document.issues == List(OpenApiIssue.Encoding("POST /upload", "multipart/form-data")),
        content(document).hcursor.downField("schema").downField("properties").downField("file").focus.isDefined,
        content(document).hcursor.downField("encoding").focus.isEmpty
      )
    ,
    test("encoding diagnostics follow media type declaration order"):
      val parts = part("file", body.binary(mediaType.pdf)).filename("one.pdf").toRecord
      val other = part("file", body.binary(mediaType.pdf)).filename("two.pdf").toRecord
      val related = mediaType("multipart", "related")
      val mixed = mediaType("multipart", "mixed")
      val document = render(
        endpoint(
          upload(
            body(related, parts) :+ body(mixed, parts) :+ body(related, other) :+ body(mixed, other)
          ),
          done
        )
      )
      assertTrue(
        document.issues == List(
          OpenApiIssue.Encoding("POST /upload", "multipart/related"),
          OpenApiIssue.Encoding("POST /upload", "multipart/mixed")
        )
      )
    ,
    test("JSON and multipart alternatives keep their own media type objects"):
      val document = render(endpoint(upload(body.json(payload.int) :+ pdf), done))
      assertTrue(
        document.issues.isEmpty,
        content(document, "application/json").hcursor.downField("schema").get[String]("type").contains("integer"),
        content(document, "application/json").hcursor.downField("encoding").focus.isEmpty,
        content(document).hcursor.downField("encoding").downField("file").focus.isDefined
      )
    ,
    test("multipart response schemas are retained and unsupported encoding is reported"):
      val document = render(endpoint(request(method.get, __ / "upload"), result(code.ok)(pdf).toUnion))
      val value = document.value.hcursor
        .downField("paths")
        .downField("/upload")
        .downField("get")
        .downField("responses")
        .downField("200")
        .downField("content")
        .downField("multipart/form-data")
      assertTrue(
        document.issues == List(OpenApiIssue.Encoding("GET /upload", "multipart/form-data")),
        value.downField("schema").downField("properties").downField("file").focus.isDefined,
        value.downField("encoding").focus.isEmpty,
        value.downField("schema").downField("encoding").focus.isEmpty
      )
    ,
    test("nested multipart parts report inner encoding instead of placing it in a property schema"):
      val nested = body.multipart(part("nested", pdf).toRecord)
      val document = render(endpoint(upload(nested), done))
      val value = content(document).hcursor
      assertTrue(
        document.issues == List(OpenApiIssue.Encoding("POST /upload", "multipart/form-data")),
        value.downField("encoding").downField("nested").get[String]("contentType").contains("multipart/form-data"),
        value.downField("schema").downField("properties").downField("nested").downField("encoding").focus.isEmpty,
        value
          .downField("schema")
          .downField("properties")
          .downField("nested")
          .downField("properties")
          .downField("file")
          .focus
          .isDefined
      )
    ,
    test("a multipart payload under a non-multipart media type reports the encoding it cannot use"):
      val parts = part("file", body.binary(mediaType.pdf)).toRecord
      val document = render(endpoint(upload(body(mediaType.json, parts)), done))
      assertTrue(
        document.issues == List(OpenApiIssue.Encoding("POST /upload", "application/json")),
        content(document, "application/json").hcursor.downField("schema").get[String]("type").contains("object"),
        content(document, "application/json").hcursor.downField("encoding").focus.isEmpty
      )
    ,
    test("names and filename examples escape quoted strings and control characters"):
      val name = "a\"b\\c"
      val file = "line\r\n\"\\.pdf"
      val parts = part(name, body.binary(mediaType.pdf)).filename(file).toRecord
      val document = render(endpoint(upload(body.multipart(parts)), done))
      val example = content(document).hcursor
        .downField("encoding")
        .downField(name)
        .downField("headers")
        .downField("Content-Disposition")
        .get[String]("example")
      assertTrue(
        document.issues.isEmpty,
        example.contains("form-data; name=\"a\\\"b\\\\c\"; filename=\"line%0D%0A\\\"\\\\.pdf\"")
      )
  )
