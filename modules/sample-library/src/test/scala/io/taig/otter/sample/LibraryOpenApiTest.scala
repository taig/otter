package io.taig.otter.sample

import io.circe.Json as CirceJson
import io.taig.otter.http.OpenApiIssue
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.codec.OpenApiPayload
import io.taig.otter.http.codec.OpenApiRenderer
import io.taig.otter.sample.api.api
import zio.Scope
import zio.test.*

/** The document the endpoints render as.
  *
  * Rendered from `api.all` -- the identical value [[LibraryRoutes]] serves from -- so nothing asserted here can be true
  * of the document and false of the server. That is the claim the module exists to make, and it is why there is no hand
  * written OpenAPI file anywhere in this project to drift out of date.
  */
object LibraryOpenApiTest extends ZIOSpecDefault:
  private val payload = OpenApiPayload.json(OpenApiProfile.V31)

  private val server = OpenApiRenderer.server(OpenApiProfile.V31, payload).render(api.Info, api.all)

  private val client = OpenApiRenderer.client(OpenApiProfile.V31, payload).render(api.Info, api.all)

  private def at(document: CirceJson, path: String*): Option[CirceJson] =
    path.foldLeft(Option(document))((json, key) => json.flatMap(_.asObject).flatMap(_.apply(key)))

  private def keys(document: CirceJson, path: String*): List[String] =
    at(document, path*).flatMap(_.asObject).map(_.keys.toList).getOrElse(Nil)

  private def strings(document: CirceJson, path: String*): List[String] =
    at(document, path*).flatMap(_.asArray).map(_.flatMap(_.asString).toList).getOrElse(Nil)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryOpenApiTest")(
    suite("the document")(
      test("says which version of the specification it is written in, and what it describes"):
        assertTrue(
          at(server.value, "openapi").flatMap(_.asString).contains("3.1.0"),
          at(server.value, "info", "title").flatMap(_.asString).contains("Otter Library")
        )
      ,
      test("holds one path per endpoint, including the ones nothing serves"):
        val paths = keys(server.value, "paths")

        assertTrue(
          paths.length == 11,
          paths.contains("/books/{isbn}"),
          paths.contains("/members/{reference}/loans"),
          paths.contains("/books/export")
        )
      ,
      test("names an operation by the id it was given rather than by one derived from the path"):
        assertTrue(
          at(server.value, "paths", "/books/{isbn}", "delete", "operationId").flatMap(_.asString).contains("deleteBook")
        )
      ,
      test("carries the tags and the summary an endpoint was annotated with"):
        assertTrue(
          strings(server.value, "paths", "/books", "get", "tags") == List("books"),
          at(server.value, "paths", "/books", "get", "summary").flatMap(_.asString).nonEmpty
        )
      ,
      /** `books.fetch` converts its union to an `Option[Book]`, which is a decision about what the handler holds. The
        * document is the place that would show it if it were anything more than that.
        */
      test("an answer converted to another type still renders every status the union named"):
        assertTrue(keys(server.value, "paths", "/books/{isbn}", "get", "responses") == List("200", "404"))
    ),
    suite("parameters")(
      test("are named by position, and a defaulted one is not required"):
        val parameters = at(server.value, "paths", "/books", "get", "parameters")
          .flatMap(_.asArray)
          .map(_.toList)
          .getOrElse(Nil)
          .flatMap: parameter =>
            for
              name <- at(parameter, "name").flatMap(_.asString)
              in <- at(parameter, "in").flatMap(_.asString)
              required <- at(parameter, "required").flatMap(_.asBoolean)
            yield (name, in, required)

        assertTrue(
          parameters.contains(("page", "query", false)),
          parameters.contains(("genre", "query", true)),
          parameters.contains(("X-Request-Id", "header", true)),
          parameters.contains(("Accept-Language", "header", false))
        )
    ),
    suite("named schemas")(
      test("are declared once under components/schemas rather than inlined at each use"):
        val declared = keys(server.value, "components", "schemas")

        assertTrue(
          declared.contains("Book"),
          declared.contains("Problem"),
          declared.contains("Category"),
          declared.length == 11
        )
      ,
      test("are referred to by $ref from the operations that use them"):
        val reference =
          at(
            server.value,
            "paths",
            "/books/{isbn}",
            "get",
            "responses",
            "200",
            "content",
            "application/json",
            "schema",
            "$ref"
          )

        assertTrue(reference.flatMap(_.asString).contains("#/components/schemas/Book"))
      ,
      test("a schema that refers to itself points at its own definition, which only a name makes possible"):
        val self = at(server.value, "components", "schemas", "Category", "properties", "shelves", "items", "$ref")

        assertTrue(self.flatMap(_.asString).contains("#/components/schemas/Category"))
    ),
    suite("the two sides are different documents")(
      test("a reader accepts an absent defaulted field where a writer always produces one"):
        val reading = strings(server.value, "components", "schemas", "BookCreate", "required")
        val writing = strings(client.value, "components", "schemas", "BookCreate", "required")

        assertTrue(
          !reading.contains("metadata"),
          writing.contains("metadata"),
          reading.length < writing.length
        )
    ),
    suite("what could not be said")(
      test("a streamed body is reported, and the rest of the document still comes back"):
        val framed = server.issues.collect { case issue: OpenApiIssue.Framed => issue }

        assertTrue(framed.length == 2, keys(server.value, "paths").length == 11)
      ,
      test("every issue names an operation, so a reader can go and look at it"):
        assertTrue(server.issues.nonEmpty, server.issues.forall(_.toString.contains("/books/")))
      ,
      test("the alphabet nothing recognises still leaves the body listed under its media type"):
        val content = keys(server.value, "paths", "/books/report", "get", "responses", "200", "content")

        assertTrue(content == List("text/csv"))
    )
  )
