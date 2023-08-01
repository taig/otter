package io.taig.crock.validation

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import munit.FunSuite

final class ValidationTest extends FunSuite:
  test("reset") {
    val constraint: Constraint[Nothing] = Constraint("x", none)
    val violation: Violation[Nothing, Long] = Violation(constraint, actual = 42L)

    val validation: Validation[Nothing, Long, String, Nothing] =
      Validation.lift[String, Int](_.length).andThen(Validation.invalidNec(violation))

    assertEquals(obtained = validation.run("foobar"), expected = violation.invalidNec)
    assertEquals(
      obtained = validation.reset.run("foobar"),
      expected = Violation(constraint, actual = "foobar").invalidNec
    )
  }

  test("tap") {
    val validation: Validation[Nothing, String, String, Unit] =
      Validation.condNec(Constraint("x", none))(_.nonEmpty)

    assertEquals(obtained = validation.run("foobar"), expected = ().valid)
    assertEquals(obtained = validation.tap.run("foobar"), expected = "foobar".valid)
  }

  test("product") {
    val int: Validation[Nothing, String, String, Int] =
      Validation.fromOptionNec(Constraint("Int", none))(_.toIntOption)
    val long: Validation[Nothing, String, String, Long] =
      Validation.fromOptionNec(Constraint("Long", none))(_.toLongOption)
    val validation: Validation[Nothing, String, String, (Int, Long)] = int.product(long)

    assertEquals(
      obtained = validation.constraints,
      expected = Chain(Constraint("Int", none), Constraint("Long", none))
    )

    assertEquals(obtained = validation.run("0"), expected = (0, 0L).valid)
    assertEquals(
      obtained = validation.run("0.0"),
      expected = NonEmptyChain
        .of(Violation(Constraint("Int", none), actual = "0.0"), Violation(Constraint("Long", none), actual = "0.0"))
        .invalid
    )
    assertEquals(
      obtained = validation.run(s"${Long.MaxValue}"),
      expected = Violation(Constraint("Int", none), s"${Long.MaxValue}").invalidNec
    )
  }
