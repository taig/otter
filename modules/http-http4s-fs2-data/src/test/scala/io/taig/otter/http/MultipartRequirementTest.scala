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

/** A multipart body asks for its parts as well as for itself.
  *
  * There is no multipart interpreter yet, and this is the test that says what one will have to be true of before it is
  * written. `Multipart.Over[S]` is the parts at the requirement their bodies hold, so an interpreter names in its own
  * type the registry it will read the parts with -- and a JSON-only registry is then rejected for an upload with a CSV
  * part, at the point the routes are built, rather than on the request that first takes that branch.
  *
  * The alternative would be to discover it afterwards. `Multipart[A]` used to widen every part to [[Body.Node]], whose
  * requirement is `Any`, and an upload written that way was servable by nothing at all; whoever wrote the first
  * interpreter would have found their own fixtures unservable and would most likely have widened `Supported` to get
  * past it, which is the one change that would make this unsound. Saying it now costs a stub codec that never runs.
  */
object MultipartRequirementTest extends ZIOSpecDefault:
  /** Parts whose bodies are read by the registry `P` covers. */
  type Parts[P[-_, +_]] = Multipart.Over[Http4sPayload.Supported[P]]

  private def unimplemented: Violations =
    Violations(Violation(constraint = Constraint.Generic.Type("multipart"), actual = "stub".asData, hint = None))

  /** A multipart interpreter in everything but the reading and writing, which this test never reaches. */
  def parts[P[-_, +_]](inner: Http4sPayload.Of[P]): Http4sPayload.Of[Parts[P]] =
    Http4sPayload(new Http4sPayload.Alphabet[Parts[P]]:
      override def select[W, R, Q[-_, +_], A](payload: Parts[P][W, R] | Q[W, R])(
          mine: Parts[P][W, R] => A,
          theirs: Q[W, R] => A
      ): A = (payload.asMatchable: @unchecked) match
        case multipart: Parts[P][W, R] @unchecked => mine(multipart)
        case other: Q[W, R] @unchecked            => theirs(other))(new Http4sPayload.Codec[Parts[P]]:
      override def decode[R](payload: Parts[P][Nothing, R], bytes: ByteVector): Validated[Violations, R] =
        Validated.invalid(MultipartRequirementTest.unimplemented)
      override def encode[W](payload: Parts[P][W, Any], value: W): Either[String, ByteVector] =
        Left("A multipart interpreter is not written yet"))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("MultipartRequirementTest")(
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
    test("the multipart interpreter alone does not cover the JSON its parts are written in"):
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
