package io.taig.otter.sample

import cats.Eq
import cats.MonadThrow
import cats.effect.Resource
import cats.syntax.all.*
import io.circe.Printer as CircePrinter
import io.taig.otter.dsl.*
import io.taig.otter.http.AppClient
import io.taig.otter.http.HttpError
import io.taig.otter.http.codec.CirceJsonPayloadDecoder
import io.taig.otter.http.codec.CirceJsonPayloadEncoder
import io.taig.otter.munit.OtterEffectSuite
import io.taig.otter.sample.app.SampleApp
import munit.Compare
import munit.FunSuite
import munit.Location
import munit.diff.Printer

import scala.collection.immutable.ListMap
import cats.effect.SyncIO

abstract class SampleSuite extends OtterEffectSuite:
  override def printer: Printer = Printer(_.toString())

  private def compare[A: Eq] = new Compare[A, A]:
    override def isEqual(obtained: A, expected: A): Boolean = obtained === expected

  given [A: Eq, B: Eq]: Eq[ListMap[A, B]] = Eq.by(_.toList)

  def assertEq[A: Eq](obtained: A, expected: A, clue: => Any = "values are not the same")(using
      location: Location
  ): Unit = assertEquals(obtained, expected, clue)(using location, compare[A])

  extension [F[_]: MonadThrow, A, B](self: F[Either[HttpError, Either[A, B]]])
    def assertSuccess(using Location): F[B] = self.flatMap:
      case Right(Right(b)) => b.pure
      case Right(Left(a))  => new IllegalStateException(s"Expected success, but got error: $a").raiseError
      case Left(error)     => new IllegalStateException(s"Expected success, but got HttpError: $error").raiseError

    def assertError(using Location): F[A] = self.flatMap:
      case Right(Left(a))  => a.pure
      case Right(Right(b)) => new IllegalStateException(s"Expected error, but got success: $b").raiseError
      case Left(error)     => new IllegalStateException(s"Expected error, but got HttpError: $error").raiseError

  val client: SyncIO[FunFixture[TestClient]] = ResourceFixture:
    Resource
      .eval(SampleApp.routes)
      .map: routes =>
        AppClient(
          decoder = CirceJsonPayloadDecoder,
          encoder = CirceJsonPayloadEncoder(printer = CircePrinter.noSpaces),
          debug = true
        )(app(routes))
      .map(TestClient.apply)
