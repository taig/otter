package io.taig.otter.http

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.http.codec.HeadersDecoder
import io.taig.otter.http.codec.HeadersEncoder
import io.taig.otter.http.codec.PathDecoder
import io.taig.otter.http.codec.PathEncoder
import io.taig.otter.http.codec.QueriesDecoder
import io.taig.otter.http.codec.QueriesEncoder
import io.taig.otter.http.fixture.*
import org.http4s.Query as Http4sQuery
import org.http4s.Uri
import zio.Scope
import zio.test.*

/** The crossing between http4s's envelope types and the slices the codecs speak.
  *
  * Every test here is the same shape on purpose: encode a value through `http`'s codec, carry it into http4s and back,
  * and read it. What is under test is the crossing and not the codec --
  * [[io.taig.otter.http.codec.EnvelopeRoundTripTest]] already says what the codecs do -- so each case is one thing the
  * crossing could quietly lose.
  */
object Http4sEnvelopeTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sEnvelopeTest")(
    suite("method")(
      test("a method not in http4s's table still crosses, because a schema may name one"):
        val method = Method("PROPFIND")

        assertTrue(Http4sEnvelope.toHttp4sMethod(method).map(Http4sEnvelope.toMethod) == Right(method))
    ),
    suite("path")(
      test("round trips through a Uri.Path"):
        val encoded = PathEncoder.encode(http.user, 42)
        val crossed = Http4sEnvelope.toPath(Http4sEnvelope.toHttp4sPath(encoded))

        assertTrue(crossed == encoded) && assertTrue(PathDecoder.decode(http.user, crossed) == Validated.valid(42))
      ,
      test("is absolute, so it is a path and not a relative reference"):
        assertTrue(Http4sEnvelope.toHttp4sPath(Vector("users", "42")).absolute)
      ,
      test("a segment holding a slash stays one segment"):
        val path = Http4sEnvelope.toHttp4sPath(Vector("files", "a/b"))

        assertTrue(path.segments.length == 2) && assertTrue(Http4sEnvelope.toPath(path) == Vector("files", "a/b"))
      ,
      test("a segment holding a space or a hash survives being put in a Uri"):
        val segments = Vector("notes", "a b#c")
        val uri = Uri(path = Http4sEnvelope.toHttp4sPath(segments))

        assertTrue(Http4sEnvelope.toPath(Uri.unsafeFromString(uri.renderString).path) == segments)
      ,
      test("an empty path is the root and reads as no segments"):
        assertTrue(Http4sEnvelope.toPath(Http4sEnvelope.toHttp4sPath(Vector.empty)) == Vector.empty)
    ),
    suite("queries")(
      test("round trips through a Query"):
        val encoded = QueriesEncoder.encode(http.listing, (Some(3), List("scala", "http")))
        val crossed = Http4sEnvelope.toQueries(Http4sEnvelope.toHttp4sQuery(encoded))

        assertTrue(crossed == encoded) &&
        assertTrue(QueriesDecoder.decode(http.listing, crossed) == Validated.valid((Some(3), List("scala", "http"))))
      ,
      test("a name given without a value stays apart from one given an empty value"):
        val query = Http4sQuery.unsafeFromString("verbose&page=")

        assertTrue(Http4sEnvelope.toQueries(query) == Chain(("verbose", None), ("page", Some(""))))
      ,
      test("a bare name reads as the flag it is, which multiParams could not have told us"):
        val queries = Http4sEnvelope.toQueries(Http4sQuery.unsafeFromString("verbose"))

        assertTrue(QueriesDecoder.decode(http.verbose, queries) == Validated.valid(true))
      ,
      test("a name given twice arrives twice and in order"):
        val queries = Http4sEnvelope.toQueries(Http4sQuery.unsafeFromString("tags=scala&tags=http"))

        assertTrue(queries == Chain(("tags", Some("scala")), ("tags", Some("http"))))
      ,
      test("a value needing escaping survives being put in a Uri"):
        val encoded = Chain(("q", Some("a b&c=d")))
        val uri = Uri(query = Http4sEnvelope.toHttp4sQuery(encoded))

        assertTrue(Http4sEnvelope.toQueries(Uri.unsafeFromString(uri.renderString).query) == encoded)
    ),
    suite("headers")(
      test("round trips through Headers"):
        val value = ("abc-123", Some(List("de", "en")))
        val encoded = HeadersEncoder.encode(http.request, value)
        val crossed = Http4sEnvelope.toHeaders(Http4sEnvelope.toHttp4sHeaders(encoded))

        assertTrue(crossed == encoded) && assertTrue(
          HeadersDecoder.decode(http.request, crossed) == Validated.valid(value)
        )
      ,
      test("a name spelled differently is still the name the schema declared"):
        val crossed = Http4sEnvelope.toHeaders(Http4sEnvelope.toHttp4sHeaders(Chain(("x-request-id", "abc-123"))))

        assertTrue(HeadersDecoder.decode(http.request, crossed) == Validated.valid(("abc-123", None)))
      ,
      test("a name given twice arrives twice, so the decoder is the one that groups it"):
        val crossed =
          Http4sEnvelope.toHeaders(
            Http4sEnvelope.toHttp4sHeaders(Chain(("Accept-Language", "de"), ("Accept-Language", "en")))
          )

        assertTrue(crossed.length == 2)
      ,
      test("a header the schema never mentions is carried across untouched"):
        assertTrue(
          Http4sEnvelope.toHeaders(Http4sEnvelope.toHttp4sHeaders(Chain(("X-Trace", "1")))) == Chain(("X-Trace", "1"))
        )
    )
  )
