package io.taig.otter

import munit.FunSuite
import cats.data.NonEmptyList
import cats.syntax.all.*

final class ViolationsTest extends FunSuite:
  test("print"):
    val violations =
      Violations.rootNec[Violation[Constraint.Any, Any]](Violation(Constraint.Type("string"), actual = "null")) |+|
        Violations.rootNec(
          Violation(Constraint.OneOf(NonEmptyList.of("foo", "bar", "baz").map(Data.String.apply)), actual = "foobar")
        ) |+|
        Violations.namespaceNec(Step.Field("foo"), Violation(Constraint.Primitive.MinLength(reference = 3), actual = 1))

    assertEquals(
      obtained = violations.print,
      expected = NonEmptyList.of(
        """$: [type "string"] ! "null"""",
        """$: [oneOf "foo,bar,baz"] ! "foobar"""",
        """$.foo: [minLength "3"] ! "1""""
      )
    )
