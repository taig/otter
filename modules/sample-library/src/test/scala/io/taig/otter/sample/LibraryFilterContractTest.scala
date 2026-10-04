package io.taig.otter.sample

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sRequestEncoder
import io.taig.otter.http.codec.OpenApiPayload
import io.taig.otter.http.codec.OpenApiRenderer
import io.taig.otter.http.codec.TypescriptEffectPayload
import io.taig.otter.http.codec.TypescriptEndpointRenderer
import io.taig.otter.sample.api.BookFilter
import io.taig.otter.sample.api.Tracing
import io.taig.otter.sample.api.api
import io.taig.otter.sample.api.books
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.json
import io.taig.otter.sample.api.schema
import scodec.bits.ByteVector
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

/** Naming the input of `GET /books` changes what its handler and its caller hold, and nothing else.
  *
  * `structural` is the endpoint as it was before [[BookFilter]] and [[Tracing]] existed, written out again rather than
  * derived from `books.list`, so the two cannot agree by sharing a mistake. Every claim below compares against it.
  */
object LibraryFilterContractTest extends ZIOSpecDefault:
  final case class Reordered(genres: List[Genre], page: Int, size: Int, available: Boolean)

  final case class Short(page: Int, size: Int, genres: List[Genre])

  final case class Mistyped(requestId: Int, languages: Option[List[String]])

  private val parameters =
    query("page", int).defaultedOnMissingOrEmpty(1) :*
      query("size", int).defaultedOnMissingOrEmpty(20) :*
      query("genre", collection.list(books.enumerated)) :*
      query.flag("available")

  private val headers =
    header("X-Request-Id", string) :* header("Accept-Language", collection.list(string)).optionalOrEmpty

  private val structural = endpoint(
    request(method.get, books.all).queries(parameters).headers(headers),
    response(status.ok)(body.json(json.collection.list(schema.book)))
  ).attr(openapi.operationId, "listBooks")
    .attr(openapi.summary, "Every book the catalogue holds")
    .attr(openapi.tags, "books")

  private val filter = BookFilter(page = 2, size = 5, genres = List(Genre.Romance, Genre.Poetry), available = true)

  private val tracing = Tracing(requestId = "abc-123", languages = Some(List("en", "de")))

  private val encoder = new Http4sRequestEncoder(Http4sCirce.Payload)

  private val decoder = new Http4sRequestDecoder(Http4sCirce.Payload)

  private val before = Chain.one[Endpoint.Declaration.Node](structural)

  private val after = Chain.one[Endpoint.Declaration.Node](books.list)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryFilterContractTest")(
    test("the named input writes the request the tuples wrote"):
      val named = encoder.encode(books.list.self.self.request, (filter, tracing))
      val tupled = encoder.encode(
        structural.self.self.request,
        (2, 5, List(Genre.Romance, Genre.Poetry), true, ("abc-123", Some(List("en", "de"))))
      )
      assertTrue(named.isRight, named == tupled)
    ,
    test("and reads it back as the named input"):
      val decoded = encoder
        .encode(books.list.self.self.request, (filter, tracing))
        .map(decoder.decode(books.list.self.self.request, _))
      assertTrue(decoded == Right(Validated.valid((filter, tracing))))
    ,
    test("an absent query parameter reaches the named input as its default"):
      val wire = Http4sWire
        .Request(Vector("books"), Chain.empty, Chain.one("X-Request-Id" -> "abc-123"), (None, ByteVector.empty))
      assertTrue(
        decoder.decode(books.list.self.self.request, wire) ==
          Validated.valid(
            (BookFilter(page = 1, size = 20, genres = Nil, available = false), Tracing("abc-123", None))
          )
      )
    ,
    test("a missing required header is still refused"):
      val wire = Http4sWire.Request(Vector("books"), Chain.empty, Chain.empty, (None, ByteVector.empty))
      assertTrue(decoder.decode(books.list.self.self.request, wire).isInvalid)
    ,
    test("naming the input preserves both OpenAPI contracts"):
      val payload = OpenApiPayload.json(OpenApiProfile.V31)
      val renderers = List(
        OpenApiRenderer.server(OpenApiProfile.V31, payload),
        OpenApiRenderer.client(OpenApiProfile.V31, payload)
      )
      assertTrue(renderers.forall: renderer =>
        val actual = renderer.render(api.Info, after)
        actual == renderer.render(api.Info, before))
    ,
    test("naming the input preserves both TypeScript contracts"):
      val renderers = List(
        TypescriptEndpointRenderer.server(TypescriptEffectPayload.json),
        TypescriptEndpointRenderer.client(TypescriptEffectPayload.json)
      )
      assertTrue(renderers.forall: renderer =>
        val actual = renderer.render(after)
        actual == renderer.render(before))
    ,
    test("a named input is checked against the parameters it names"):
      assertTrue(
        typeChecks("parameters.to[BookFilter]"),
        typeChecks("headers.to[Tracing]"),
        !typeChecks("parameters.to[LibraryFilterContractTest.Reordered]"),
        !typeChecks("parameters.to[LibraryFilterContractTest.Short]"),
        !typeChecks("headers.to[LibraryFilterContractTest.Mistyped]")
      )
  )
