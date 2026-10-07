package io.taig.otter.sample

import cats.data.Chain
import cats.data.Validated
import cats.effect.unsafe.implicits.global
import io.circe.parser.parse
import io.taig.otter.codec.JsonCirceDecoder
import io.taig.otter.codec.JsonCirceEncoder
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sRequestEncoder
import io.taig.otter.sample.api.BookFilter
import io.taig.otter.sample.api.Tracing
import io.taig.otter.sample.api.books
import io.taig.otter.sample.api.schema
import scodec.bits.ByteVector
import zio.Scope
import zio.test.*

object GenreContractTest extends ZIOSpecDefault:
  private val genres = List(
    Genre.Biography -> "biography",
    Genre.Children -> "children",
    Genre.Fantasy -> "fantasy",
    Genre.History -> "history",
    Genre.Poetry -> "poetry",
    Genre.Romance -> "romance",
    Genre.Thriller -> "thriller"
  )

  private val requestDecoder = new Http4sRequestDecoder[cats.effect.IO, io.taig.otter.Json.Node](Http4sCirce.Payload)

  private val requestEncoder = new Http4sRequestEncoder[cats.effect.IO, io.taig.otter.Json.Node](Http4sCirce.Payload)

  private def query(value: List[String]): Http4sWire.Request =
    Http4sWire.Request(
      Vector("books"),
      Chain.fromSeq(value.map("genre" -> Some(_))),
      Chain.one("X-Request-Id" -> "contract-test"),
      (None, ByteVector.empty)
    )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("GenreContractTest")(
    test("the explicit mapping covers every genre"):
      assertTrue(genres.map(_._1).toSet == Genre.values.toSet, genres.size == Genre.values.length)
    ,
    test("JSON accepts and emits each documented spelling"):
      assertTrue(genres.forall: (genre, spelling) =>
        val document = parse(s"\"$spelling\"").toOption.get
        JsonCirceDecoder.decode(schema.genre, document) == Validated.valid(genre) &&
        JsonCirceEncoder.encode(schema.genre, genre).noSpaces == s"\"$spelling\"")
    ,
    test("JSON rejects unknown and differently cased spellings"):
      assertTrue(
        JsonCirceDecoder.decode(schema.genre, parse("\"unknown\"").toOption.get).isInvalid,
        JsonCirceDecoder.decode(schema.genre, parse("\"Fantasy\"").toOption.get).isInvalid
      )
    ,
    test("the books query accepts and emits each documented spelling in order"):
      val values = genres.map(_._1)
      val spellings = genres.map(_._2)
      val filter = BookFilter(page = 1, size = 20, genres = values, available = false)
      val tracing = Tracing(requestId = "contract-test", languages = None)
      val encoded = requestEncoder.encode(books.list.self.self.request, (filter, tracing)).unsafeRunSync()
      val decoded = requestDecoder.decode(books.list.self.self.request, query(spellings)).unsafeRunSync()

      assertTrue(
        encoded.exists(_.queries.toList.collect { case ("genre", Some(value)) => value } == spellings),
        decoded == Validated.valid((filter, tracing))
      )
    ,
    test("the books query rejects unknown and differently cased spellings"):
      assertTrue(
        requestDecoder.decode(books.list.self.self.request, query(List("unknown"))).unsafeRunSync().isInvalid,
        requestDecoder.decode(books.list.self.self.request, query(List("Fantasy"))).unsafeRunSync().isInvalid
      )
  )
