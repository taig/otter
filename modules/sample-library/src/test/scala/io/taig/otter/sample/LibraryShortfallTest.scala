package io.taig.otter.sample

import io.taig.otter.Json
import io.taig.otter.http.*
import io.taig.otter.sample.api.Deleted
import io.taig.otter.sample.api.books
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.schema
import scodec.bits.ByteVector
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeCheckErrors
import scala.compiletime.testing.typeChecks

/** The endpoints this repository can describe and cannot carry.
  *
  * They are here because being told is the feature. A backend that met a payload it did not understand could quietly
  * send an empty body, or throw something with no name on it; this one is not handed the endpoint at all. Each claim
  * below is made of the compiler, and the requirement on the interpreter argument is what refuses it. `OpenApiIssue`
  * and `TypescriptIssue` take the same stand where a document is wanted for an endpoint nothing here can serve.
  *
  * Every claim below is therefore a *guarantee* and not a regret: what changes when a streamed body becomes servable is
  * that these tests fail, which is exactly when somebody should look at them.
  */
object LibraryShortfallTest extends ZIOSpecDefault:
  final case class Upload(metadata: Book.Patch, image: Option[ByteVector])

  val convertedStream: Endpoint.Of[Body.Streamed.Requirement[Json.Node], Unit, Deleted.Removed.type] = endpoint(
    request(method.get, __ / "stream"),
    response(status.ok)(body.ndjson(schema.book)).to[Deleted.Removed.type]
  )

  val convertedMultipart: Endpoint.Of[Multipart.Requirement[Payload], Unit, LibraryShortfallTest.Upload] = endpoint(
    request(method.get, __ / "multipart"),
    response(status.ok)(body.multipart(books.cover)).to[LibraryShortfallTest.Upload]
  )

  private inline val MultipartRoute = """
    import cats.effect.IO
    import io.taig.otter.http.*
    import io.taig.otter.sample.*
    import io.taig.otter.sample.api.books
    import scodec.bits.ByteVector
    Http4s.routes[IO](Route(books.upload,
      (_: (Isbn, (Book.Patch, Option[ByteVector]))) => IO.unit))(Http4sCirce.Payload)
  """

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryShortfallTest")(
    test("converting a streamed response preserves its unsupported requirement"):
      val errors = typeCheckErrors("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.sample.LibraryShortfallTest
        import io.taig.otter.sample.api.Deleted
        Http4s.routes[IO](Route(LibraryShortfallTest.convertedStream,
          (_: Unit) => IO.pure(Deleted.Removed)))(Http4sCirce.Payload)
      """)
      assertTrue(errors.exists(_.message.contains("Streamed")))
    ,
    test("converting a multipart response preserves its unsupported requirement"):
      val errors = typeCheckErrors("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.sample.LibraryShortfallTest
        Http4s.routes[IO](Route(LibraryShortfallTest.convertedMultipart,
          (_: Unit) => IO.raiseError[LibraryShortfallTest.Upload](new IllegalStateException("unused"))))(Http4sCirce.Payload)
      """)
      assertTrue(errors.exists(_.message.contains("Multipart")))
    ,
    test("a multipart payload cannot be served without an interpreter"):
      assertTrue(!typeChecks(LibraryShortfallTest.MultipartRoute))
    ,
    test("a streamed answer cannot be served by a buffered backend"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.sample.api.books
        Http4s.routes[IO](Route(books.exported, (_: Unit) => IO.unit))(Http4sCirce.Payload)
      """))
    ,
    test("a CSV stream cannot be served by a buffered backend"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.sample.api.books
        Http4s.routes[IO](Route(books.report, (_: Unit) => IO.unit))(Http4sCirce.Payload)
      """))
    ,
    test("the compiler identifies the unsupported requirement"):
      val errors = typeCheckErrors(LibraryShortfallTest.MultipartRoute)
      assertTrue(errors.exists(_.message.contains("Multipart")))
  )
