package io.taig.otter.sample

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.Http4sFailure
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.MediaType
import io.taig.otter.http.Route
import io.taig.otter.sample.api.books
import org.http4s.Uri
import org.http4s.client.Client as Http4sClient
import org.http4s.implicits.*
import scodec.bits.ByteVector
import zio.Exit
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

/** The endpoints this repository can describe and cannot carry.
  *
  * They are here because being told is the feature. A backend that met a payload it did not understand could quietly
  * send an empty body, or throw something with no name on it; this one stops and says which media type it was and on
  * which endpoint. `OpenApiIssue` and `TypescriptIssue` take the same stand as values, and `Http4sFailure.Interpreter`
  * is that stand where an effect is available to take it in.
  *
  * Every claim below is therefore a *guarantee* and not a regret: what changes when a streamed body becomes servable is
  * that these tests fail, which is exactly when somebody should look at them.
  */
object LibraryShortfallTest extends ZIOSpecDefault:
  private val Base: Uri = uri"http://library.test"

  /** The unserved endpoint, mounted on its own, so that reaching it is not a routing accident. */
  private def attempt[A, B](endpoint: Endpoint[A, B], handler: A => IO[B])(value: A): Task[B] =
    ZIO.fromFuture: _ =>
      val client = Http4sClient.fromHttpApp(Http4s.routes[IO](Http4sCirce.Payload)(Route(endpoint, handler)).orNotFound)

      Http4s.client[IO, A, B](Http4sCirce.Payload, Base, client)(endpoint)(value).unsafeToFuture()

  private def issues(exit: Exit[Throwable, Any]): List[Http4sIssue] =
    exit.causeOption.toList.flatMap(_.failures).collect { case Http4sFailure.Interpreter(issue) => issue }

  private val patch: Book.Patch = Book.Patch(None, None, None, None)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryShortfallTest")(
    test("a multipart payload is reported, and names the media type nothing recognised"):
      attempt(books.upload, (_: (Isbn, (Book.Patch, Option[ByteVector]))) => IO.unit)(
        (Isbn.digits("9780261102217"), (patch, None))
      ).exit.map: exit =>
        assertTrue(issues(exit).exists:
          case Http4sIssue.Uninterpreted(mediaType) => mediaType == MediaType.MultipartFormData
          case _                                    => false)
    ,
    test("a streamed answer is reported rather than answered with an empty body"):
      attempt(books.exported, (_: Unit) => IO.unit)(()).exit.map(exit => assertTrue(exit.isFailure))
    ,
    test("a stream whose elements are written in an alphabet nothing interprets is reported too"):
      attempt(books.report, (_: Unit) => IO.unit)(()).exit.map(exit => assertTrue(exit.isFailure))
    ,
    test("what is reported is a named issue and not an arbitrary exception"):
      attempt(books.upload, (_: (Isbn, (Book.Patch, Option[ByteVector]))) => IO.unit)(
        (Isbn.digits("9780261102217"), (patch, None))
      ).exit.map(exit => assertTrue(issues(exit).nonEmpty))
  )
