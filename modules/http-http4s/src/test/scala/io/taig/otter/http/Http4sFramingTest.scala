package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.Chunk
import fs2.Stream
import io.taig.otter.http.codec.Http4sFraming
import io.taig.otter.http.codec.Http4sStreamFailure
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

object Http4sFramingTest extends ZIOSpecDefault:
  private def run[A](io: IO[A]): Task[A] = ZIO.fromFuture(_ => io.unsafeToFuture())
  private def bytes(value: String): ByteVector =
    ByteVector.view(value.getBytes(java.nio.charset.StandardCharsets.UTF_8))
  private def decode(frame: Frame, value: String, size: Int, limit: Int = 1048576): IO[List[String]] =
    Stream
      .emits(bytes(value).toArray.toList.grouped(size).toList)
      .flatMap(group => Stream.chunk(Chunk.from(group)))
      .covary[IO]
      .through(Http4sFraming.decode(frame, limit))
      .compile
      .toList
      .flatMap(_.traverse(_.decodeUtf8.liftTo[IO]))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sFramingTest")(
    test("LF, CRLF and final unterminated records survive every chunk boundary"):
      run((1 to 20).toList.traverse(size => decode(Frame.Lines, "\"héllo\"\r\n2\n3", size)))
        .map(results => assertTrue(results.forall(_ == List("\"héllo\"", "2", "3"))))
    ,
    test("literal multibyte delimiters, interior empty frames and final data survive chunk boundaries"):
      run((1 to 15).toList.traverse(size => decode(Frame.Delimited("💧END"), "a💧END💧ENDb", size)))
        .map(results => assertTrue(results.forall(_ == List("a", "", "b"))))
    ,
    test("SSE handles BOM, all line endings, comments, data joining, metadata and incomplete EOF"):
      val input =
        "\ufeff: comment\r\nevent: update\rid: 2\nretry: 1000\ndata: first\r\ndata:second\r\n\r\n:keepalive\n\ndata:\n\ndata: incomplete"
      run((1 to 20).toList.traverse(size => decode(Frame.Events, input, size)))
        .map(results => assertTrue(results.forall(_ == List("first\nsecond", ""))))
    ,
    test("SSE dispatches a CR-terminated event without pulling the next chunk"):
      run(
        (Stream.emits(bytes("data: ready\r\r").toArray).covary[IO] ++ Stream.never[IO])
          .through(Http4sFraming.decode(Frame.Events, 100))
          .take(1)
          .compile
          .toList
      )
        .map(result => assertTrue(result == List(bytes("ready"))))
    ,
    test("frame boundaries allow exactly the configured payload size"):
      run(List("1234\n", "1234\r\n", "1234").traverse(value => decode(Frame.Lines, value, 1, 4)))
        .map(results => assertTrue(results.forall(_ == List("1234"))))
    ,
    test("empty streams and trailing terminators produce no phantom record"):
      run(List(Frame.Lines, Frame.Events, Frame.Delimited("END")).traverse(frame => decode(frame, "", 1)))
        .map(results => assertTrue(results.forall(_.isEmpty)))
    ,
    test("blank NDJSON records and lone CR are rejected"):
      run(List("\n", "\r\n", "a\rb\n", "a\r").traverse(value => decode(Frame.Lines, value, 1).attempt))
        .map(results => assertTrue(results.forall(_.isLeft)))
    ,
    test("the frame limit stops an unterminated source without reading the rest"):
      run(for
        count <- IO.ref(0)
        source = Stream.repeatEval(count.update(_ + 1).as('x'.toByte))
        result <- source.through(Http4sFraming.decode(Frame.Delimited("END"), 8)).compile.drain.attempt
        read <- count.get
      yield assertTrue(result.isLeft, read <= 12))
    ,
    test("SSE limits include ignored fields and accumulated multiline data"):
      run(
        List("unknown: 123456789\n\n", "data: a\ndata: b\n\n", ":1234567890123456789\n\n")
          .traverse(value => decode(Frame.Events, value, 1, 12).attempt)
      )
        .map(results => assertTrue(results.forall(_.isLeft)))
    ,
    test("line and delimiter limit failures name the element index"):
      run(decode(Frame.Lines, "1\n123456789\n", 1, 4).attempt)
        .map(result =>
          assertTrue(result.left.exists {
            case failure: Http4sStreamFailure => failure.index == 1
            case _                            => false
          })
        )
    ,
    test("encoders emit exact boundaries and reject collisions including separator overlaps"):
      assertTrue(
        Http4sFraming.encode(Frame.Lines, 100, bytes("1"), 0).map(_.decodeUtf8) == Right(Right("1\n")),
        Http4sFraming.encode(Frame.Events, 100, bytes("first\nsecond"), 0).map(_.decodeUtf8) == Right(
          Right("data: first\ndata: second\n\n")
        ),
        Http4sFraming.encode(Frame.Delimited("END"), 100, bytes("a"), 0).map(_.decodeUtf8) == Right(Right("aEND")),
        Http4sFraming.encode(Frame.Delimited("aba"), 100, bytes("ab"), 0).isLeft,
        Http4sFraming.encode(Frame.Lines, 100, bytes("a\nb"), 0).isLeft,
        Http4sFraming.encode(Frame.Lines, 2, bytes("123"), 0).isLeft
      )
  ) @@ TestAspect.timeout(zio.Duration.fromSeconds(20))
