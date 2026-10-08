package io.taig.otter.sample

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sEnvelope
import io.taig.otter.sample.api.api
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.implicits.*
import zio.Scope
import zio.ZIO
import zio.test.*

object LibraryExportTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryExportTest")(
    test("the NDJSON export is served and consumed through the shared endpoint"):
      ZIO.fromFuture(_ =>
        (for
          library <- Library[IO]()
          expected <- library.exported.flatMap(_.compile.toList)
          app = LibraryRoutes(library)
          transport = Client.fromHttpApp(app)
          client = Http4s.client(LibraryRoutes.payload, uri"http://library.test", transport).withApi(api.all)
          result <- client.resource(api.default.streaming.exported)(()).use {
            case Right(values) => values.compile.toList
            case Left(error)   => IO.raiseError(new IllegalStateException(error.toString))
          }
          response <- app(Request[IO](uri = uri"http://library.test/books/export"))
          wire <- Http4sEnvelope.toBytes(response.entity)
        yield assertTrue(
          result == expected,
          result.map(_.isbn.value) == result.map(_.isbn.value).sorted,
          response.status.code == 200,
          Http4sEnvelope.toMediaType(response.headers).exists(_.render == "application/x-ndjson"),
          wire.decodeUtf8.toOption.exists(_.linesIterator.size == expected.size)
        )).unsafeToFuture()
      )
  )
