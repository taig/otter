package io.taig.otter.http

import cats.effect.IO
import cats.effect.Resource
import cats.effect.unsafe.implicits.global
import fs2.Stream
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import org.http4s.Entity
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.Response as Http4sResponse
import org.http4s.Uri
import org.http4s.client.Client as Http4sClient
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

object Http4sClientTest extends ZIOSpecDefault:
  private val base: Uri = uri"http://otter.test/prefix"
  private val empty = endpoint(request(method.get, __ / "empty"), response(status.noContent))
  private val text = endpoint(request(method.get, __ / "text"), response(status.ok)(body.json(payload.string)))
  private val cause = new IllegalStateException("transport failed")

  private def run[A](value: IO[A]): Task[A] = ZIO.fromFuture(_ => value.unsafeToFuture())

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sClientTest")(
    test("configuration and function selection are inert, and repeated calls share the transport"):
      run(for
        paths <- IO.ref(List.empty[String])
        released <- IO.ref(0)
        transport = Http4sClient[IO](request =>
          Resource.make(
            paths.update(_ :+ request.uri.path.renderString).as(Http4sResponse[IO](org.http4s.Status.NoContent))
          )(_ => released.update(_ + 1))
        )
        client = Http4s.client(Http4sPayload.Empty, base, transport)
        plain = client(empty)
        configured = client.withApi(Api(ErrorPolicy.default, UnroutedPolicy.default))
        declared = configured(empty)
        before <- paths.get
        _ <- plain(())
        answer <- declared(())
        _ <- plain(())
        after <- paths.get
        releases <- released.get
      yield assertTrue(before.isEmpty, after == List.fill(3)("/prefix/empty"), answer == Right(()), releases == 3))
    ,
    test("transport failures stay in the effect even under an API policy"):
      val transport = Http4sClient[IO](_ => Resource.eval(IO.raiseError[Http4sResponse[IO]](cause)))
      val client = Http4s
        .client(Http4sPayload.Empty, base, transport)
        .withApi(Api(ErrorPolicy.default, UnroutedPolicy.default))
      run(client(empty)(()).attempt).map(answer => assertTrue(answer == Left(cause)))
    ,
    test("malformed responses fail decoding and release the response resource"):
      run(for
        released <- IO.ref(false)
        response = Http4sResponse[IO](
          headers = Http4sHeaders(Http4sHeader.Raw(CIString("Content-Type"), "application/json")),
          entity = Entity.strict(ByteVector.encodeUtf8("not JSON").toOption.get)
        )
        transport = Http4sClient[IO](_ => Resource.make(IO.pure(response))(_ => released.set(true)))
        client = Http4s
          .client(Http4sCirce.Payload, base, transport)
          .withApi(Api(ErrorPolicy.default, UnroutedPolicy.default))
        result <- client(text)(()).attempt
        finalized <- released.get
      yield assertTrue(result.left.exists { case _: Http4sFailure.Response => true; case _ => false }, finalized))
    ,
    test("entity read failures stay in the effect and release the response resource"):
      run(for
        released <- IO.ref(false)
        response = Http4sResponse[IO](entity = Entity.Streamed(Stream.raiseError[IO](cause), None))
        transport = Http4sClient[IO](_ => Resource.make(IO.pure(response))(_ => released.set(true)))
        client = Http4s.client(Http4sCirce.Payload, base, transport)
        result <- client(text)(()).attempt
        finalized <- released.get
      yield assertTrue(result == Left(cause), finalized))
    ,
    test("cancelling a response read cancels the call and releases its resource"):
      run(for
        started <- IO.deferred[Unit]
        released <- IO.ref(false)
        response = Http4sResponse[IO](
          entity = Entity.Streamed(Stream.eval(started.complete(())).drain ++ Stream.never[IO], None)
        )
        transport = Http4sClient[IO](_ => Resource.make(IO.pure(response))(_ => released.set(true)))
        client = Http4s.client(Http4sCirce.Payload, base, transport)
        fiber <- client(text)(()).start
        _ <- started.get
        _ <- fiber.cancel
        outcome <- fiber.join
        finalized <- released.get
      yield assertTrue(outcome.isCanceled, finalized))
    ,
    test("invalid methods fail before acquiring the transport"):
      val invalid = endpoint(request(Method("invalid method"), __), response(status.noContent))
      run(for
        called <- IO.ref(false)
        transport = Http4sClient[IO](_ => Resource.eval(called.set(true).as(Http4sResponse[IO]())))
        client = Http4s.client(Http4sPayload.Empty, base, transport)
        result <- client(invalid)(()).attempt
        acquired <- called.get
      yield assertTrue(result.left.exists { case _: Http4sFailure.Method => true; case _ => false }, !acquired))
  )
