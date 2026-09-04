package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.http.fixture.*
import zio.Scope
import zio.test.*

/** The envelope is the part of a request that is text and nothing more, so the whole of it round trips here with no
  * effect type, no backend and no bytes in sight.
  */
object EnvelopeRoundTripTest extends ZIOSpecDefault:
  private def pairs(values: (String, String)*): Chain[(String, Option[String])] =
    Chain.fromSeq(values.map((name, value) => (name, Some(value))))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("EnvelopeRoundTripTest")(
    suite("path")(
      test("writes its literals and reads back what stood between them"):
        val encoded = PathEncoder.encode(http.user, 42)

        assertTrue(encoded == Vector("users", "42")) &&
        assertTrue(PathDecoder.decode(http.user, encoded) == Validated.valid(42))
      ,
      test("a literal after a placeholder drops out of the value too"):
        val encoded = PathEncoder.encode(http.posts, 7)

        assertTrue(encoded == Vector("users", "7", "posts")) &&
        assertTrue(PathDecoder.decode(http.posts, encoded) == Validated.valid(7))
      ,
      test("a segment too many is not a match"):
        assertTrue(PathDecoder.decode(http.user, Vector("users", "42", "extra")).isInvalid)
      ,
      test("a segment too few is not a match"):
        assertTrue(PathDecoder.decode(http.user, Vector("users")).isInvalid)
      ,
      test("a literal spelled differently is not a match"):
        assertTrue(PathDecoder.decode(http.user, Vector("posts", "42")).isInvalid)
      ,
      test("a placeholder that does not hold what it describes reports at its own name"):
        val result = PathDecoder.decode(http.user, Vector("users", "not-a-number"))

        assertTrue(result.isInvalid) && assertTrue(result.leftMap(_.toString).swap.exists(_.contains("id")))
      ,
      test("round trips any id"):
        check(Gen.int)(id =>
          val encoded = PathEncoder.encode(http.user, id)
          assertTrue(PathDecoder.decode(http.user, encoded) == Validated.valid(id))
        )
    ),
    suite("queries")(
      test("a repetition is one name given again for each element"):
        val encoded = QueriesEncoder.encode(http.listing, (Some(3), List("scala", "http")))

        assertTrue(encoded == pairs("page" -> "3", "tags" -> "scala", "tags" -> "http")) &&
        assertTrue(QueriesDecoder.decode(http.listing, encoded) == Validated.valid((Some(3), List("scala", "http"))))
      ,
      test("an absent parameter drops its name"):
        val encoded = QueriesEncoder.encode(http.listing, (None, Nil))

        assertTrue(encoded == Chain.empty) &&
        assertTrue(QueriesDecoder.decode(http.listing, encoded) == Validated.valid((None, Nil)))
      ,
      test("a default stands in for a name that was not given"):
        assertTrue(QueriesDecoder.decode(http.paged, Chain.empty) == Validated.valid(1)) &&
        assertTrue(QueriesDecoder.decode(http.paged, pairs("page" -> "5")) == Validated.valid(5))
      ,
      test("a name given without a value reads as the flag it is"):
        assertTrue(QueriesDecoder.decode(http.verbose, Chain.one(("verbose", None))) == Validated.valid(true)) &&
        assertTrue(QueriesDecoder.decode(http.verbose, Chain.empty) == Validated.valid(false)) &&
        assertTrue(QueriesDecoder.decode(http.verbose, pairs("verbose" -> "1")) == Validated.valid(true)) &&
        assertTrue(QueriesDecoder.decode(http.verbose, pairs("verbose" -> "no")) == Validated.valid(false))
      ,
      test("a name the schema never mentions is left where it is"):
        val values = pairs("page" -> "2", "utm_source" -> "newsletter")

        assertTrue(QueriesDecoder.decode(http.paged, values) == Validated.valid(2))
      ,
      test("a parameter that is not a repetition refuses to be given twice"):
        assertTrue(QueriesDecoder.decode(http.paged, pairs("page" -> "1", "page" -> "2")).isInvalid)
      ,
      test("round trips any listing"):
        check(Gen.option(Gen.int), Gen.listOf(Gen.alphaNumericString.filter(_.nonEmpty)))((page, tags) =>
          val encoded = QueriesEncoder.encode(http.listing, (page, tags))
          assertTrue(QueriesDecoder.decode(http.listing, encoded) == Validated.valid((page, tags)))
        )
    ),
    suite("headers")(
      test("a list valued header is one line with the values joined"):
        val encoded = HeadersEncoder.encode(http.request, ("abc", Some(List("en", "de"))))

        assertTrue(encoded == Chain(("X-Request-Id", "abc"), ("Accept-Language", "en,de"))) &&
        assertTrue(HeadersDecoder.decode(http.request, encoded) == Validated.valid(("abc", Some(List("en", "de")))))
      ,
      test("a name spelled in another case is the same name"):
        val values = Chain(("x-request-id", "abc"), ("ACCEPT-LANGUAGE", "en, de"))

        assertTrue(HeadersDecoder.decode(http.request, values) == Validated.valid(("abc", Some(List("en", "de")))))
      ,
      test("a header the schema never mentions is left where it is"):
        val values = Chain(("X-Request-Id", "abc"), ("X-Forwarded-For", "10.0.0.1"))

        assertTrue(HeadersDecoder.decode(http.request, values) == Validated.valid(("abc", None)))
      ,
      test("a header that has to be there and is not fails"):
        assertTrue(HeadersDecoder.decode(http.request, Chain.empty).isInvalid)
    )
  )
