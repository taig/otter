package io.taig.validation

import cats.data.{Chain, NonEmptyChain, NonEmptyList}
import cats.syntax.all.*
import io.taig.validation.syntax.*
import munit.FunSuite

final class ValidationTest extends FunSuite:
  test("reset") {
    val constraint: Constraint[Nothing] = constraints(identifiers("x"))
    val violation: Violation[Nothing, Long] = constraint.toViolation(actual = 42L)

    val validation: Validation[Nothing, Long, String, Nothing] =
      Validation.fromFunction[String, Int](_.length).andThen(Validation.invalidNec(violation))

    assertEquals(obtained = validation.run("foobar"), expected = violation.invalidNec)
    assertEquals(
      obtained = validation.reset.run("foobar"),
      expected = constraint.toViolation(actual = "foobar").invalidNec
    )
  }

  test("tap") {
    val validation: Validation[Nothing, String, String, Unit] =
      Validation.condNec(constraints(identifiers("x")))(_.nonEmpty)

    assertEquals(obtained = validation.run("foobar"), expected = ().valid)
    assertEquals(obtained = validation.tap.run("foobar"), expected = "foobar".valid)
  }

  test("product") {
    val int: Validation[Nothing, String, String, Int] =
      Validation.fromOptionNec(constraints(identifiers("int")))(_.toIntOption)
    val long: Validation[Nothing, String, String, Long] =
      Validation.fromOptionNec(constraints(identifiers("long")))(_.toLongOption)
    val validation: Validation[Nothing, String, String, (Int, Long)] = int.product(long)

    assertEquals(
      obtained = validation.constraints,
      expected = Chain(constraints(identifiers("int")), constraints(identifiers("long")))
    )

    assertEquals(obtained = validation.run("0"), expected = (0, 0L).valid)
    assertEquals(
      obtained = validation.run("0.0"),
      expected = NonEmptyChain
        .of(
          constraints(identifiers("int")).toViolation("0.0"),
          constraints(identifiers("long")).toViolation("0.0")
        )
        .invalid
    )
    assertEquals(
      obtained = validation.run(s"${Long.MaxValue}"),
      expected = constraints(identifiers("int")).toViolation(s"${Long.MaxValue}").invalidNec
    )
  }

  test("orElse") {
    val int: Validation[Nothing, String, String, Int] =
      Validation.fromOptionNec(constraints(identifiers("int")))(_.toIntOption)
    val long: Validation[Nothing, String, String, Long] =
      Validation.fromOptionNec(constraints(identifiers("long")))(_.toLongOption)
    val validation: Validation[Nothing, String, String, Either[Int, Long]] = int orElse long

    assertEquals(
      obtained = validation.constraints,
      expected = Chain(Constraint.Or(Chain(constraints(identifiers("int"))), Chain(constraints(identifiers("long")))))
    )

    assertEquals(obtained = validation.run("0"), expected = 0.asLeft.valid)
    assertEquals(obtained = validation.run(s"${Long.MaxValue}"), expected = Long.MaxValue.asRight.valid)

    assertEquals(
      obtained = validation.run("0.0"),
      expected = NonEmptyChain
        .of(
          constraints(identifiers("int")).toViolation(actual = "0.0"),
          constraints(identifiers("long")).toViolation(actual = "0.0")
        )
        .invalid
    )
  }
