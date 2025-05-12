package io.taig.otter.sample

import cats.Eq
import cats.MonadThrow
import cats.effect.Resource
import cats.syntax.all.*
import io.circe.Printer as CircePrinter
import io.taig.otter.dsl.*
import io.taig.otter.http.AppClient
import io.taig.otter.http.CirceJsonPayloadDecoder
import io.taig.otter.http.CirceJsonPayloadEncoder
import io.taig.otter.munit.OtterEffectSuite
import io.taig.otter.sample.app.SampleApp
import munit.Compare
import munit.FunSuite
import munit.Location
import munit.diff.Printer

import scala.collection.immutable.ListMap

abstract class SampleSuite extends OtterEffectSuite:
  override def printer: Printer = Printer(_.toString())

  private def compare[A: Eq] = new Compare[A, A]:
    override def isEqual(obtained: A, expected: A): Boolean = obtained === expected

  given [A: Eq, B: Eq]: Eq[ListMap[A, B]] = Eq.by(_.toList)

  def assertEq[A: Eq](obtained: A, expected: A, clue: => Any = "values are not the same")(using
      location: Location
  ): Unit = assertEquals(obtained, expected, clue)(using location, compare[A])

  extension [F[_]: MonadThrow, A, B](self: F[Either[A, B]])
    def assertSuccess(using Location): F[B] = self.flatMap:
      case Right(b) => b.pure
      case Left(a)  => new IllegalStateException(s"Expected Right, but got Left: $a").raiseError

    def assertError(using Location): F[A] = self.flatMap:
      case Left(a)  => a.pure
      case Right(b) => new IllegalStateException(s"Expected Left, but got Right: $b").raiseError

  val client = ResourceFixture:
    Resource
      .eval(SampleApp.routes)
      .map: routes =>
        AppClient(
          decoder = CirceJsonPayloadDecoder.Default,
          encoder = CirceJsonPayloadEncoder(printer = CircePrinter.noSpaces),
          debug = true
        )(app(routes))
