package io.taig.otter.http

import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.taig.otter.Field
import io.taig.otter.Json
import io.taig.otter.http.codec.Http4sBodyDecoder
import io.taig.otter.http.codec.Http4sBodyEncoder
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.fixture.dsl
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload as json
import org.http4s.Entity
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.Header
import org.http4s.Headers as HttpHeaders
import org.http4s.Request as HttpRequest
import org.http4s.client.Client
import org.http4s.implicits.*
import org.http4s.multipart.Boundary
import org.http4s.multipart.Multipart as HttpParts
import org.http4s.multipart.Part as HttpPart
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

import scala.compiletime.testing.typeChecks

object Http4sMultipartTest extends ZIOSpecDefault:
  final case class Details(title: String, count: Int)
  final case class Upload(metadata: Http4sMultipartTest.Details, image: Option[ByteVector])

  private val details =
    (json.field("title", json.string) :* json.field("count", json.int)).to[Http4sMultipartTest.Details]
  val parts: Multipart[Http4sPayload.Supported[Json.Node], Http4sMultipartTest.Upload] =
    (part("metadata", body.json(details)) :*
      part("image", body.binary(dsl.mediaType.octetStream)).filename("cover.png").optional)
      .to[Http4sMultipartTest.Upload]
  val upload =
    endpoint(request(method.post, __ / "upload")(body.multipart(parts)), response(status.ok)(body.multipart(parts)))
  val payload = Http4sCirce.Payload.orElse(Http4sMultipart.payload(Http4sCirce.Payload))
  private val value =
    Http4sMultipartTest.Upload(Http4sMultipartTest.Details("Cover", 2), Some(ByteVector(0, 255, 13, 10)))
  private val reader =
    new Http4sBodyDecoder[IO, Http4sMultipart.Parts[Json.Node], Nothing](Http4sMultipart.payload(Http4sCirce.Payload))
  private val writer =
    new Http4sBodyEncoder[IO, Http4sMultipart.Parts[Json.Node], Nothing](Http4sMultipart.payload(Http4sCirce.Payload))

  /** Fail the test if a buffered schema starts producing a streamed entity. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  private def strictBytes(entity: Entity[IO]): ByteVector = entity match
    case Entity.Strict(bytes) => bytes
    case Entity.Empty         => ByteVector.empty
    case _                    => throw new IllegalStateException("Multipart must remain buffered")

  private def run[A](io: IO[A]): Task[A] = ZIO.fromFuture(_ => io.unsafeToFuture())
  private def bytes(text: String): ByteVector = ByteVector.view(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
  private def rawPart(name: String, text: String, contentType: Option[String] = None): HttpPart[IO] =
    HttpPart
      .formData(name, text)
      .copy(headers =
        HttpHeaders(
          org.http4s.headers.`Content-Disposition`("form-data", Map(CIString("name") -> name))
        ) ++ HttpHeaders(contentType.toList.map(value => Header.Raw(CIString("Content-Type"), value)))
      )

  private def wire(parts: HttpPart[IO]*): IO[(Option[MediaType], Entity[IO])] =
    val multipart = HttpParts(parts.toVector, Boundary("external-boundary"))
    Http4sEnvelope
      .toBytes(EntityEncoder.multipartEncoder[IO].toEntity(multipart))
      .map(bytes => (Http4sEnvelope.toMediaType(multipart.headers), Entity.strict(bytes)))

  private def decode(parts: HttpPart[IO]*) =
    wire(parts*).flatMap(reader.decodeDetailed(body.multipart(Http4sMultipartTest.parts), _))

  private val metadata = rawPart("metadata", """{"title":"Cover","count":2}""", Some("application/json; charset=utf-8"))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sMultipartTest")(
    test("JSON and optional binary parts round trip through requests and responses without a socket"):
      val transport = Client.fromHttpApp(Http4s.app[IO](Route(upload, IO.pure[Http4sMultipartTest.Upload]))(payload))
      val call = Http4s.client(payload, uri"http://multipart.test", transport)(upload)
      val values = List(value, value.copy(image = None), value.copy(image = Some(ByteVector.empty)))
      run(values.traverse(call)).map(results => assertTrue(results == values))
    ,
    test("http4s reads the emitted names, media types, filename, boundary and binary bytes"):
      run(
        writer
          .encode(body.multipart(parts), value)
          .flatMap(_.leftMap(issue => new IllegalStateException(issue.show)).liftTo[IO])
          .flatMap: (mediaType, bytes) =>
            val request = HttpRequest[IO](
              headers = HttpHeaders(Header.Raw(CIString("Content-Type"), mediaType.render)),
              entity = bytes
            )
            EntityDecoder
              .multipart[IO]
              .decode(request, strict = true)
              .value
              .flatMap(_.liftTo[IO])
              .flatMap: decoded =>
                decoded.parts.traverse(part =>
                  Http4sEnvelope
                    .toBytes(part.entity)
                    .map((part.name, part.filename, Http4sEnvelope.toMediaType(part.headers), _))
                )
      ).map(parts =>
        assertTrue(
          parts.map(_._1) == Vector(Some("metadata"), Some("image")),
          parts(1)._2.contains("cover.png"),
          parts(0)._3.contains(dsl.mediaType.json),
          parts(1)._3.contains(dsl.mediaType.octetStream),
          parts(1)._4 == value.image.get
        )
      )
    ,
    test("http4s-built parts may be reordered, omit content type, and use any filename"):
      val image = HttpPart.fileData[IO]("image", "different.bin", Entity.strict(value.image.get))
      run((decode(image, rawPart("extra", "ignored"), metadata), decode(rawPart("image", ""), metadata)).tupled)
        .map((named, unnamed) =>
          assertTrue(
            named.toOption.contains(value),
            unnamed.toOption.contains(value.copy(image = Some(ByteVector.empty)))
          )
        )
    ,
    test("missing optional parts decode as None, and missing required parts identify the name"):
      run((decode(metadata), decode(rawPart("extra", "ignored"))).tupled).map((present, missing) =>
        assertTrue(
          present.toOption.contains(value.copy(image = None)),
          missing.swap.toOption.exists(failure =>
            failure.category == Failure.Category.Validation && Http4s.report(failure.violations).contains(".metadata")
          )
        )
      )
    ,
    test("part failures accumulate with their request body paths and categories"):
      val decoder = new Http4sRequestDecoder[IO, Body.Or[Json.Node, Http4sMultipart.Parts[Json.Node]], Nothing](payload)
      run(
        wire(
          rawPart("metadata", """{"title":1,"count":"bad"}""", Some("application/json")),
          rawPart("image", "bad", Some("text/plain"))
        )
          .flatMap(body =>
            decoder.decodeDetailed(upload.request, Http4sWire.Request(Vector("upload"), Chain.empty, Chain.empty, body))
          )
      )
        .map(result =>
          assertTrue(
            result.swap.toOption.exists(failure =>
              failure.category == Failure.Category.Validation &&
                List("$.body.metadata.title", "$.body.metadata.count", "$.body.image").forall(
                  Http4s.report(failure.violations).contains
                )
            )
          )
        )
    ,
    test("wrong content type and malformed JSON retain their distinct categories"):
      run(
        (
          decode(rawPart("metadata", "{}", Some("text/plain"))),
          decode(rawPart("metadata", "not json", Some("application/json")))
        ).tupled
      )
        .map((wrong, syntax) =>
          assertTrue(
            wrong.swap.toOption.exists(_.category == Failure.Category.ContentType),
            syntax.swap.toOption.exists(_.category == Failure.Category.Syntax)
          )
        )
    ,
    test("missing, invalid and truncated boundaries are syntax failures; quoted boundaries are preserved"):
      val declared = body.multipart(parts)
      val malformed = List(
        (Some(dsl.mediaType.multipartFormData), bytes("garbage")),
        (None, bytes("garbage")),
        (Some(dsl.mediaType.multipartFormData.withParameter("boundary", "")), bytes("garbage")),
        (
          Some(dsl.mediaType.multipartFormData.withParameter("boundary", "x")),
          bytes("--x\r\nContent-Disposition: form-data; name=metadata\r\n\r\n{}")
        )
      )
      run(malformed.traverse((media, bytes) => reader.decodeDetailed(declared, (media, Entity.strict(bytes))))).map(
        results =>
          assertTrue(
            results.forall(_.swap.toOption.exists(_.category == Failure.Category.Syntax)),
            Http4sEnvelope
              .toMediaType("Multipart/Form-Data; boundary=\"with space\"")
              .flatMap(_.parameter("boundary"))
              .contains("with space")
          )
      )
    ,
    test("duplicate schema names consume parts in arrival order"):
      val repeated = part("x", body.json(json.int)) :* part("x", body.json(json.string))
      run(
        wire(rawPart("x", "1"), rawPart("x", "\"second\""))
          .flatMap(reader.decodeDetailed(body.multipart(repeated), _))
      ).map(result => assertTrue(result.toOption.contains((1, "second"))))
    ,
    test("an invalid duplicate part is consumed before the next declaration is decoded"):
      val repeated = part("x", body.json(json.int)) :* part("x", body.json(json.int))
      run(
        wire(rawPart("x", "\"bad\""), rawPart("x", "42"))
          .flatMap(reader.decodeDetailed(body.multipart(repeated), _))
      )
        .map(result =>
          assertTrue(result.swap.toOption.exists(failure => Http4s.report(failure.violations).linesIterator.size == 1))
        )
    ,
    test("defaults, explicit empty contracts and an entirely omitted product preserve absence"):
      val optional = part("x", body.binary).optional.toRecord
      val defaulted = part("x", body.binary).defaulted(bytes("default")).toRecord
      val empty = part("x", body.binary).optional(Field.Presence.EmptyOrMissing).toRecord
      run(for
        encoded <- writer
          .encode(body.multipart(optional), None)
          .flatMap(_.leftMap(issue => new IllegalStateException(issue.show)).liftTo[IO])
        omitted <- reader.decodeDetailed(body.multipart(optional), (Some(encoded._1), encoded._2))
        default <- reader.decodeDetailed(body.multipart(defaulted), (Some(encoded._1), encoded._2))
        explicit <- writer
          .encode(body.multipart(empty), None)
          .flatMap(_.leftMap(issue => new IllegalStateException(issue.show)).liftTo[IO])
        absent <- reader.decodeDetailed(body.multipart(empty), (Some(explicit._1), explicit._2))
      yield (encoded, omitted, default, explicit, absent)).map((encoded, omitted, default, explicit, absent) =>
        assertTrue(
          strictBytes(encoded._2).decodeUtf8.toOption.exists(_.endsWith("--\r\n")),
          omitted.toOption.contains(None),
          default.toOption.contains(bytes("default")),
          absent.toOption.contains(None),
          strictBytes(explicit._2).decodeUtf8.toOption.exists(_.contains("name=\"x\""))
        )
      )
    ,
    test("empty-triggered defaults differ from missing-only defaults and defaulted fields always write"):
      val emptyDefault = part("x", body.binary).defaulted(bytes("default"), Field.Absent.Empty).toRecord
      val missingDefault = part("x", body.binary).defaulted(bytes("default")).toRecord
      run(for
        input <- wire(rawPart("x", ""))
        empty <- reader.decodeDetailed(body.multipart(emptyDefault), input)
        missing <- reader.decodeDetailed(body.multipart(missingDefault), input)
        output <- writer
          .encode(body.multipart(emptyDefault), bytes("written"))
          .flatMap(_.leftMap(issue => new IllegalStateException(issue.show)).liftTo[IO])
        read <- reader.decodeDetailed(body.multipart(emptyDefault), (Some(output._1), output._2))
      yield (empty, missing, read)).map((empty, missing, read) =>
        assertTrue(
          empty.toOption.contains(bytes("default")),
          missing.toOption.contains(ByteVector.empty),
          read.toOption.contains(bytes("written"))
        )
      )
    ,
    test("a quoted incoming boundary is used to parse the entity"):
      run(
        wire(metadata).flatMap((_, bytes) =>
          reader.decodeDetailed(
            body.multipart(parts),
            (Http4sEnvelope.toMediaType("multipart/form-data; boundary=\"external-boundary\""), bytes)
          )
        )
      )
        .map(result => assertTrue(result.toOption.contains(value.copy(image = None))))
    ,
    test("header parameters are quoted and boundary collisions do not corrupt file bytes"):
      val schema = part("a\"b", body.binary).filename("a\\b\"c.bin").toRecord
      val file = bytes("\r\n--otter-boundary-0\r\n")
      run(
        writer
          .encode(body.multipart(schema), file)
          .flatMap(_.leftMap(issue => new IllegalStateException(issue.show)).liftTo[IO])
          .flatMap: (mediaType, encoded) =>
            reader.decodeDetailed(body.multipart(schema), (Some(mediaType), encoded)).map((mediaType, encoded, _))
      ).map((mediaType, encoded, result) =>
        assertTrue(
          mediaType.parameter("boundary").contains("otter-boundary-1"),
          result.toOption.contains(file),
          strictBytes(encoded).decodeUtf8.toOption.exists(_.contains("filename=\"a\\\\b\\\"c.bin\""))
        )
      )
    ,
    test("explicitly registered nested multipart bodies share the same interpreter"):
      val nested = part("nested", body.multipart(parts)).toRecord
      val endpoint = io.taig.otter.http.fixture.dsl
        .endpoint(request(method.post, __)(body.multipart(nested)), response(status.ok)(body.multipart(nested)))
      val interpreter = Http4sCirce.Payload.orElse(Http4sMultipart.payload(payload))
      val transport =
        Client.fromHttpApp(Http4s.app[IO](Route(endpoint, IO.pure[Http4sMultipartTest.Upload]))(interpreter))
      run(Http4s.client(interpreter, uri"http://multipart.test", transport)(endpoint)(value)).map(result =>
        assertTrue(result == value)
      )
    ,
    test("a configured client requires multipart registration and still rejects streamed parts"):
      assertTrue(
        typeChecks("""
          val transport = org.http4s.client.Client.fromHttpApp(Http4s.app[IO](Route(upload, IO.pure[Upload]))(payload))
          Http4s.client(payload, uri"http://multipart.test", transport)(upload)
        """),
        !typeChecks("""
          val transport = org.http4s.client.Client.fromHttpApp(Http4s.app[IO](Route(upload, IO.pure[Upload]))(payload))
          Http4s.client(Http4sCirce.Payload, uri"http://multipart.test", transport)(upload)
        """),
        !typeChecks("""
          val streamed = part("rows", body.ndjson[fs2.Stream[IO, +*]](json.string)).toRecord
          val endpoint = io.taig.otter.http.fixture.dsl.endpoint(request(method.post, __)(body.multipart(streamed)), response(status.noContent))
          Http4s.app[IO](Route(endpoint, (_: Unit) => IO.unit))(payload)
        """)
      )
  )
