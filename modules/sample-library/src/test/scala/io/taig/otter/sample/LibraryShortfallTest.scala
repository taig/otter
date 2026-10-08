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
  * Each negative check names an absent registration. Registering buffered JSON never grants JSON streaming, and the
  * sample's JSON stream registration never grants CSV streaming.
  */
object LibraryShortfallTest extends ZIOSpecDefault:
  final case class Upload(metadata: Book.Patch, image: Option[ByteVector])

  final case class Exported(values: fs2.Stream[cats.effect.IO, Book])
  val convertedStream = endpoint(
    request(method.get, __ / "stream"),
    response(status.ok)(body.ndjson[fs2.Stream[cats.effect.IO, +*]](schema.book)).to[LibraryShortfallTest.Exported]
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
      (_: (Isbn, (Book.Patch, Option[ByteVector]))) => IO.pure(io.taig.otter.sample.api.Uploaded.Stored)))(Http4sCirce.Payload)
  """

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryShortfallTest")(
    test("converting a streamed response preserves its unsupported requirement"):
      val errors = typeCheckErrors("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.sample.LibraryShortfallTest
        import io.taig.otter.sample.api.Deleted
        Http4s.routes[IO](Route(LibraryShortfallTest.convertedStream,
          (_: Unit) => IO.pure(LibraryShortfallTest.Exported(fs2.Stream.empty))))(Http4sCirce.Payload)
      """)
      assertTrue(
        errors.exists(error =>
          error.message.contains("Http4sInterpreter") && error.lineContent.contains("Http4sCirce.Payload")
        )
      )
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
        Http4s.routes[IO](Route(io.taig.otter.sample.api.api.default.streaming.exported, (_: Unit) => IO.pure(fs2.Stream.empty[IO])))(Http4sCirce.Payload)
      """))
    ,
    test("a CSV stream cannot be served by the JSON stream registration"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.sample.api.books
        Http4s.routes[IO](Route(io.taig.otter.sample.api.api.default.streaming.report, (_: Unit) => IO.pure(fs2.Stream.empty[IO])))(io.taig.otter.sample.LibraryRoutes.payload)
      """))
    ,
    test("the missing multipart interpreter is rejected at the interpreter argument"):
      val errors = typeCheckErrors(LibraryShortfallTest.MultipartRoute)
      assertTrue(
        errors.exists(error =>
          error.message.contains("Http4sInterpreter") && error.lineContent.contains("Http4sCirce.Payload")
        )
      )
    ,
    test("the cover upload and a converted multipart response work with explicit registration"):
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.sample.*
        import io.taig.otter.sample.api.books
        Http4s.routes[IO](
          Route(books.upload, (value: (Isbn, (Book.Patch, Option[scodec.bits.ByteVector]))) => IO.pure[io.taig.otter.sample.api.Uploaded](io.taig.otter.sample.api.Uploaded.Stored)),
          Route(LibraryShortfallTest.convertedMultipart, (_: Unit) => IO.raiseError[LibraryShortfallTest.Upload](new IllegalStateException("unused")))
        )(LibraryRoutes.payload)
      """))
  )
