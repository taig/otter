package io.taig.otter

import munit.FunSuite
import munit.diff.Printer
import cats.Eq
import cats.syntax.all.*
import munit.Compare
import munit.Location

abstract class OtterSuite extends FunSuite:
  override def printer: Printer = Printer(_.toString())

  private def compare[A: Eq] = new Compare[A, A]:
    override def isEqual(obtained: A, expected: A): Boolean = obtained === expected

  def assertEq[A: Eq](obtained: A, expected: A, clue: => Any = "values are not the same")(using
      location: Location
  ): Unit = assertEquals(obtained, expected, clue)(using location, compare[A])
