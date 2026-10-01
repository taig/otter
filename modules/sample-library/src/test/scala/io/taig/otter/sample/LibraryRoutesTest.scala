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
  * Four different things go wrong here and they get four different answers, which is the distinction the router exists
  * to draw: a path nothing describes is a `404`, a path something describes under another method is a `405`, a path
  * this endpoint describes but the request does not hold is a `400`, and a body that parsed and then broke the schema
  * is a `422`. The first two are "you wanted someone else"; the other two are "you wanted me, and got it wrong" -- and
  * only the last could be answered at all without reading the envelope twice.
  *
  * Every one of them comes back as a [[Problem]] document, which is the whole reason the API declares an error policy
  * and an unrouted policy rather than leaving either to the interpreter.
  */
object LibraryRoutesTest extends ZIOSpecDefault:
  /** The status and the body together, so an answer can be asked both questions at once. */
  private def answer(request: Http4sRequest[IO]): Task[(Int, String)] =
    respond(request).map((code, _, body) => (code, body))

  /** The status, the `Allow` header and the body, for the answers where which methods a path takes is the point. */
  private def respond(request: Http4sRequest[IO]): Task[(Int, Option[String], String)] =
    ZIO.fromFuture: _ =>
      Library[IO]()
        .flatMap: library =>
          LibraryRoutes(library)
            .run(request)
            .flatMap: response =>
              Http4sEnvelope
                .toBytes(response.entity)
                .map: bytes =>
                  val allow = response.headers.headers.collectFirst:
                    case header if header.name == CIString("Allow") => header.value
                  (response.status.code, allow, bytes.decodeUtf8.getOrElse(""))
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
    test("deletion writes an empty 204"):
      answer(Http4sRequest[IO](method = Http4sMethod.DELETE, uri = uri"http://library.test/books/9780261102217"))
        .map((code, body) => assertTrue(code == 204, body.isEmpty))
    ,
    test("a deletion conflict writes a JSON problem under 409"):
      ZIO.fromFuture: _ =>
        Library[IO]()
          .flatMap: library =>
            library
              .borrow(
                java.util.UUID.fromString("6f2a5c1e-0b3d-4f7a-9c8e-1d2b3a4c5d6e"),
                Loan.Request(Isbn.digits("9780261102217"), None)
              )
              .flatMap: _ =>
                LibraryRoutes(library)
                  .run(
                    Http4sRequest[IO](method = Http4sMethod.DELETE, uri = uri"http://library.test/books/9780261102217")
                  )
                  .flatMap: response =>
                    Http4sEnvelope
                      .toBytes(response.entity)
                      .map: bytes =>
                        val problem = io.circe.parser.parse(bytes.decodeUtf8.getOrElse("")).toOption
                        assertTrue(
                          response.status.code == 409,
                          response.headers.headers.exists(header =>
                            header.name == CIString("Content-Type") && header.value == "application/json"
                          ),
                          problem.flatMap(_.hcursor.get[String]("kind").toOption).contains("conflict"),
                          problem.flatMap(_.hcursor.get[String]("title").toOption).contains("9780261102217 is on loan")
                        )
          .unsafeToFuture()
    ,
    suite("a path no endpoint describes")(
      test("is the API's own not found, and not a bad request"):
        respond(get(uri"http://library.test/orders/42")).map((code, allow, body) =>
          assertTrue(code == 404, allow.isEmpty, body.contains("\"kind\":\"unrouted\""))
        )
      ,
      test("a segment too many is a different path rather than a malformed one"):
        answer(get(uri"http://library.test/books/9780261102217/pages/2")).map((code, body) =>
          assertTrue(code == 404, body.contains("\"kind\":\"unrouted\""))
        )
    ),
    suite("a path an endpoint describes under another method")(
      test("the method is part of what a route matches on, and the answer names the methods that are"):
        respond(Http4sRequest[IO](method = Http4sMethod.PUT, uri = uri"http://library.test/books/9780261102217"))
          .map((code, allow, body) =>
            assertTrue(
              code == 405,
              allow.contains("GET, PATCH, DELETE"),
              body.contains("\"kind\":\"unrouted\""),
              body.contains("\"detail\":[\"GET\",\"PATCH\",\"DELETE\"]")
            )
          )
      ,
      test("the path is matched on its arity and literals alone, so a value that does not parse is still a 405"):
        respond(Http4sRequest[IO](method = Http4sMethod.PUT, uri = uri"http://library.test/books/not-an-isbn"))
          .map((code, allow, _) => assertTrue(code == 405, allow.contains("GET, PATCH, DELETE")))
      ,
      test("a collection lists its own methods"):
        respond(Http4sRequest[IO](method = Http4sMethod.DELETE, uri = uri"http://library.test/books"))
          .map((code, allow, _) => assertTrue(code == 405, allow.contains("GET, POST")))
      ,
      test("a literal path lists the one method it takes"):
        respond(Http4sRequest[IO](method = Http4sMethod.DELETE, uri = uri"http://library.test/health"))
          .map((code, allow, _) => assertTrue(code == 405, allow.contains("GET")))
    ),
    suite("a request this API described but did not hold")(
      test("a path segment that does not parse is a bad request, and says where"):
        answer(get(uri"http://library.test/books/not-an-isbn")).map((code, body) =>
          assertTrue(code == 400, body.contains("$.path"), body.contains("isbn"), body.contains("\"malformed\""))
        )
      ,
      test("a missing required header is a bad request and names the header"):
        answer(get(uri"http://library.test/books")).map((code, body) =>
          assertTrue(code == 400, body.contains("X-Request-Id"), body.contains("\"kind\":\"malformed\""))
        )
      ,
      test("a query that does not hold what it describes is a bad request"):
        answer(Http4sRequest[IO](uri = uri"http://library.test/books?page=soon", headers = tracing))
          .map((code, body) => assertTrue(code == 400, body.contains("$.query.page")))
      ,
      test("a body that parses and breaks the schema is a malformed problem with status 422"):
        answer(
          json(
            Http4sMethod.POST,
            uri"http://library.test/books",
            """{"isbn":"9780000000000","title":"","pages":0,"published":"2020-01-01"}"""
          )
        )
          .map((code, body) =>
            assertTrue(
              code == 422,
              body.contains("$.body.title"),
              body.contains("$.body.pages"),
              body.contains("\"kind\":\"malformed\"")
            )
          )
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
      test("a body that is not a document is a syntax failure"):
        answer(json(Http4sMethod.POST, uri"http://library.test/books", "not json"))
          .map((code, body) => assertTrue(code == 400, body.contains("\"kind\":\"malformed\"")))
      ,
      test("an unsupported content type is a malformed problem with status 415"):
        answer(
          Http4sRequest[IO](
            method = Http4sMethod.POST,
            uri = uri"http://library.test/books",
            headers = Http4sHeaders(Http4sHeader.Raw(CIString("Content-Type"), "application/pdf")),
            entity = Entity.strict(ByteVector(0x25, 0x50, 0x44, 0x46))
          )
        ).map((code, body) => assertTrue(code == 415, body.contains("\"kind\":\"malformed\"")))
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
      test("so is a delete, by /books/{isbn} under the same method"):
        answer(Http4sRequest[IO](method = Http4sMethod.DELETE, uri = uri"http://library.test/books/export"))
          .map((code, body) => assertTrue(code == 400, body.contains("isbn")))
      ,
      test("and the shadowing decides which methods a method not allowed names"):
        respond(Http4sRequest[IO](method = Http4sMethod.PUT, uri = uri"http://library.test/books/export"))
          .map((code, allow, _) => assertTrue(code == 405, allow.contains("GET, PATCH, DELETE")))
      ,
      test("a literal one segment longer is not shadowed, and is not found"):
        answer(Http4sRequest[IO](method = Http4sMethod.POST, uri = uri"http://library.test/books/9780261102217/cover"))
          .map((code, body) => assertTrue(code == 404, body.contains("\"kind\":\"unrouted\"")))
    )
  )
