package io.taig.otter.http

import cats.data.Validated
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.http.codec.Http4sPayload
import io.taig.validation.Violation
import scodec.bits.ByteVector
import zio.Scope
import zio.test.*

import scala.compiletime.asMatchable
import scala.compiletime.testing.typeChecks

/** Multipart registration covers exactly the alphabets supplied for its parts. */
object MultipartRequirementTest extends ZIOSpecDefault:
  def parts[P[-_, +_]](inner: Http4sPayload.Of[P]): Http4sPayload.Of[Http4sMultipart.Parts[P]] =
    Http4sMultipart.payload(inner)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("MultipartRequirementTest")(
    test("different multipart capabilities cannot be hidden behind the same erased alphabet"):
      val json = Http4sMultipart.payload(Http4sCirce.Payload)
      val csv = Http4sMultipart.payload(Http4sFs2Data.Payload)
      assertTrue(
        scala.util.Try(json.orElse(csv)).failed.toOption.exists(_.getMessage.contains("Register multipart once")),
        scala.util.Try(Http4sCirce.Payload.orElse(json).orElse(csv)).isFailure
      )
    ,
    test("an upload whose parts are all JSON is served by a JSON registry and a JSON multipart interpreter"):
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.Upload
        import io.taig.otter.http.fixture.api
        Http4s.routes[IO](Route(api.create, (_: Upload) => IO.pure(io.taig.otter.http.fixture.Report("r", 1))))(
          Http4sCirce.Payload.orElse(MultipartRequirementTest.parts(Http4sCirce.Payload))
        )
      """))
    ,
    test("a multipart interpreter does not cover standalone JSON responses"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.Upload
        import io.taig.otter.http.fixture.api
        Http4s.routes[IO](Route(api.create, (_: Upload) => IO.pure(io.taig.otter.http.fixture.Report("r", 1))))(
          MultipartRequirementTest.parts(Http4sCirce.Payload)
        )
      """))
    ,
    test("a JSON registry alone does not cover the multipart structure"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.Upload
        import io.taig.otter.http.fixture.api
        Http4s.routes[IO](Route(api.create, (_: Upload) => IO.pure(io.taig.otter.http.fixture.Report("r", 1))))(
          Http4sCirce.Payload
        )
      """))
  )
