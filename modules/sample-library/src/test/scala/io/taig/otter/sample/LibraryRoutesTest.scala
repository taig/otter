package io.taig.otter.sample

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.taig.otter.http.Http4sEnvelope
import org.http4s.Entity
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.Method as Http4sMethod
import org.http4s.Request as Http4sRequest
import org.http4s.Uri
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

/** What the routes answer a request they did not describe.
  *
  * Three different things go wrong here and they get three different answers, which is the distinction the router
  * exists to draw: a path nothing describes is a `404`, a path this endpoint describes but the request does not hold is
  * a `400`, and a body that parsed and then broke the schema is a `422`. The first is "you wanted someone else"; the
  * other two are "you wanted me, and got it wrong" -- and only the third could be answered at all without reading the
  * envelope twice.
  *
  * Every one of them comes back as a [[Problem]] document, which is the whole reason `Http4s.routes` takes a renderer
  * for a malformed request rather than deciding for itself.
  */
object LibraryRoutesTest extends ZIOSpecDefault:
  /** The status and the body together, so an answer can be asked both questions at once. */
  private def answer(request: Http4sRequest[IO]): Task[(Int, String)] =
    ZIO.fromFuture: _ =>
      Library[IO]()
        .flatMap: library =>
          LibraryRoutes(library).orNotFound
            .run(request)
            .flatMap: response =>
              Http4sEnvelope
                .toBytes(response.entity)
                .map(bytes => (response.status.code, bytes.decodeUtf8.getOrElse("")))
        .unsafeToFuture()

  private def get(uri: Uri): Http4sRequest[IO] = Http4sRequest[IO](uri = uri)

  private def json(method: Http4sMethod, uri: Uri, body: String): Http4sRequest[IO] =
    Http4sRequest[IO](
      method = method,
      uri = uri,
      headers = Http4sHeaders(Http4sHeader.Raw(CIString("Content-Type"), "application/json")),
      entity = Entity.strict(ByteVector.encodeUtf8(body).getOrElse(ByteVector.empty))
    )

  private val tracing: Http4sHeaders = Http4sHeaders(Http4sHeader.Raw(CIString("X-Request-Id"), "abc-123"))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryRoutesTest")(
    suite("a path no endpoint describes")(
      test("falls through, and is not a bad request"):
        answer(get(uri"http://library.test/orders/42")).map((code, _) => assertTrue(code == 404))
      ,
      test("a segment too many is a different path rather than a malformed one"):
        answer(get(uri"http://library.test/books/9780261102217/pages/2")).map((code, _) => assertTrue(code == 404))
      ,
      test("the method is part of what a route matches on"):
        answer(Http4sRequest[IO](method = Http4sMethod.PUT, uri = uri"http://library.test/books/9780261102217"))
          .map((code, _) => assertTrue(code == 404))
    ),
    suite("a request this API described but did not hold")(
      test("a path segment that does not parse is a bad request, and says where"):
        answer(get(uri"http://library.test/books/not-an-isbn")).map((code, body) =>
          assertTrue(code == 400, body.contains("$.path"), body.contains("isbn"), body.contains("\"malformed\""))
        )
      ,
      test("a missing required header is a bad request and names the header"):
        answer(get(uri"http://library.test/books")).map((code, body) =>
          assertTrue(code == 400, body.contains("X-Request-Id"))
        )
      ,
      test("a query that does not hold what it describes is a bad request"):
        answer(Http4sRequest[IO](uri = uri"http://library.test/books?page=soon", headers = tracing))
          .map((code, body) => assertTrue(code == 400, body.contains("$.query.page")))
      ,
      test("a body that parses and breaks the schema is unprocessable, not malformed"):
        answer(
          json(
            Http4sMethod.POST,
            uri"http://library.test/books",
            """{"isbn":"9780000000000","title":"","pages":0,"published":"2020-01-01"}"""
          )
        )
          .map((code, body) => assertTrue(code == 422, body.contains("$.body.title"), body.contains("$.body.pages")))
      ,
      test("a refinement on a collection is checked like any other, and names the field"):
        val genres = List.fill(11)("\"poetry\"").mkString(",")

        answer(
          json(
            Http4sMethod.POST,
            uri"http://library.test/books",
            s"""{"isbn":"9780000000000","title":"Too Many","pages":1,"published":"2020-01-01","genres":[$genres]}"""
          )
        ).map((code, body) => assertTrue(code == 422, body.contains("$.body.genres")))
      ,
      test("a body that is not a document at all is still the content, so still unprocessable"):
        answer(json(Http4sMethod.POST, uri"http://library.test/books", "not json"))
          .map((code, _) => assertTrue(code == 422))
      ,
      test("a violation in the envelope alongside one in the body drops the answer back to a bad request"):
        answer(json(Http4sMethod.PATCH, uri"http://library.test/books/nope", """{"pages":0}"""))
          .map((code, body) => assertTrue(code == 400, body.contains("$.path")))
    ),
    suite("the answer is this API's own document")(
      test("a malformed request is a Problem, with one line of detail per violation"):
        answer(get(uri"http://library.test/books/not-an-isbn")).map((code, body) =>
          assertTrue(
            code == 400,
            body.contains("\"kind\":\"malformed\""),
            body.contains("\"detail\":["),
            body.contains("The request does not hold what this endpoint describes")
          )
        )
      ,
      test("a handler that refuses answers with the same shape the decoder does"):
        answer(
          json(
            Http4sMethod.POST,
            uri"http://library.test/books",
            """{"isbn":"9780261102217","title":"Again","pages":1,"published":"2020-01-01"}"""
          )
        )
          .map((code, body) => assertTrue(code == 409, body.contains("\"kind\":\"conflict\"")))
    ),
    suite("a placeholder shadows a literal of the same arity")(
      test("/books/export is caught by /books/{isbn} and reported as an ISBN that does not parse"):
        answer(get(uri"http://library.test/books/export")).map((code, body) =>
          assertTrue(code == 400, body.contains("isbn"))
        )
      ,
      test("a literal one segment longer is not shadowed, and falls through"):
        answer(Http4sRequest[IO](method = Http4sMethod.POST, uri = uri"http://library.test/books/9780261102217/cover"))
          .map((code, _) => assertTrue(code == 404))
    )
  )
