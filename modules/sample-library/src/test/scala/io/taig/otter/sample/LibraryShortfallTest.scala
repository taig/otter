package io.taig.otter.sample

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
  private inline val Multipart = """
    import cats.effect.IO
    import io.taig.otter.http.*
    import io.taig.otter.sample.*
    import io.taig.otter.sample.api.books
    import scodec.bits.ByteVector
    Http4s.routes[IO](Route(books.upload,
      (_: (Isbn, (Book.Patch, Option[ByteVector]))) => IO.unit))(Http4sCirce.Payload)
  """

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryShortfallTest")(
    test("a multipart payload cannot be served without an interpreter"):
      assertTrue(!typeChecks(LibraryShortfallTest.Multipart))
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
      val errors = typeCheckErrors(LibraryShortfallTest.Multipart)
      assertTrue(errors.exists(_.message.contains("Multipart")))
  )
