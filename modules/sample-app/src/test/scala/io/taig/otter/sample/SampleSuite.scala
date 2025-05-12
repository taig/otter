package io.taig.otter.sample

import cats.Eq
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

  val client = ResourceFixture:
    Resource
      .eval(SampleApp.routes)
      .map: routes =>
        AppClient(
          decoder = CirceJsonPayloadDecoder.Default,
          encoder = CirceJsonPayloadEncoder(printer = CircePrinter.noSpaces),
          debug = true
        )(app(routes))
