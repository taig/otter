package io.taig.otter.sample

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.github.iltotore.iron.autoRefine
import io.taig.otter.http.Http4s
import io.taig.otter.sample.api.Deleted
import io.taig.otter.sample.api.Uploaded
import io.taig.otter.sample.api.api
import io.taig.otter.sample.api.books
import org.http4s.HttpApp
import org.http4s.client.Client
import org.http4s.implicits.*
import scodec.bits.ByteVector
import zio.Scope
import zio.ZIO
import zio.test.*

object LibraryCoverTest extends ZIOSpecDefault:
  private val isbn = Isbn.digits("9780261102217")
  private val patch = Book.Patch(Some("New cover"), None, None, None)
  private val unchanged = Book.Patch(None, None, None, None)
  private val image = ByteVector(0, 255, 13, 10)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryCoverTest")(
    test("the served client uploads metadata and stores, preserves and replaces cover bytes"):
      ZIO.fromFuture(_ =>
        (for
          library <- Library[IO]()
          statuses <- IO.ref(List.empty[Int])
          app = LibraryRoutes(library)
          transport = Client.fromHttpApp(
            HttpApp[IO](request => app(request).flatTap(response => statuses.update(_ :+ response.status.code)))
          )
          client = Http4s.client(LibraryRoutes.payload, uri"http://library.test", transport).withApi(api.all)
          stored <- client(books.upload)((isbn, (patch, Some(image))))
          book <- library.fetch(isbn)
          first <- library.cover(isbn)
          omitted <- client(books.upload)((isbn, (unchanged, None)))
          preserved <- library.cover(isbn)
          empty <- client(books.upload)((isbn, (unchanged, Some(ByteVector.empty))))
          replaced <- library.cover(isbn)
          seen <- statuses.get
        yield assertTrue(
          stored == Right(Uploaded.Stored),
          omitted == Right(Uploaded.Stored),
          empty == Right(Uploaded.Stored),
          seen == List(204, 204, 204),
          book.exists(_.title == "New cover"),
          first.contains(image),
          preserved == first,
          replaced.contains(ByteVector.empty)
        )).unsafeToFuture()
      )
    ,
    test("an unknown ISBN returns the declared JSON 404 and creates neither a book nor a cover"):
      val unknown = Isbn.digits("9780000000001")
      ZIO.fromFuture(_ =>
        (for
          library <- Library[IO]()
          status <- IO.ref(0)
          app = LibraryRoutes(library)
          transport = Client.fromHttpApp(
            HttpApp[IO](request => app(request).flatTap(response => status.set(response.status.code)))
          )
          result <- Http4s
            .client(LibraryRoutes.payload, uri"http://library.test", transport)
            .withApi(api.all)(books.upload)((unknown, (patch, Some(image))))
          book <- library.fetch(unknown)
          cover <- library.cover(unknown)
          seen <- status.get
        yield assertTrue(
          result == Right(Uploaded.Missing(Problem.missing(s"${unknown.value} is not in the catalogue"))),
          seen == 404,
          book.isEmpty,
          cover.isEmpty
        )).unsafeToFuture()
      )
    ,
    test("successful deletion removes a cover and rejected deletion preserves it"):
      ZIO.fromFuture(_ =>
        (for
          removable <- Library[IO]()
          _ <- removable.upload(isbn, (patch, Some(image)))
          removed <- removable.delete(isbn)
          gone <- removable.cover(isbn)
          borrowed <- Library[IO]()
          _ <- borrowed.upload(isbn, (patch, Some(image)))
          _ <- borrowed.borrow(Library.State.Seed.members.head._1, Loan.Request(isbn, None))
          refused <- borrowed.delete(isbn)
          retained <- borrowed.cover(isbn)
        yield assertTrue(
          removed == Deleted.Removed,
          gone.isEmpty,
          refused != Deleted.Removed,
          retained.contains(image)
        )).unsafeToFuture()
      )
  )
