package io.taig.otter

import cats.Eq
import cats.syntax.all.*
import munit.Compare
import munit.FunSuite
import munit.Location
import munit.diff.Printer

import scala.collection.immutable.ListMap

abstract class OtterSuite extends FunSuite:
  override def printer: Printer = Printer(_.toString())

  private def compare[A: Eq] = new Compare[A, A]:
    override def isEqual(obtained: A, expected: A): Boolean = obtained === expected

  given [A: Eq, B: Eq]: Eq[ListMap[A, B]] = Eq.by(_.toList)

  def assertEq[A: Eq](obtained: A, expected: A, clue: => Any = "values are not the same")(using
      location: Location
  ): Unit = assertEquals(obtained, expected, clue)(using location, compare[A])
