package io.taig.otter

import munit.FunSuite
import cats.syntax.all.*

final class ViolationsTest extends FunSuite:
  val violations = Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String("null"))) |+|
    Violations.rootNec(Violation.oneOf(List("foo", "bar", "baz"), "foobar")) |+|
    Violations.namespaceNec(
      XPath.Root / "foo",
      Violation(Constraint.Primitive.MinLength(reference = 3), actual = Data.Number(1))
    )

  test("show"):
    assertEquals(
      obtained = violations.show,
      expected = """$: type "string" ! "null"
                   |$: oneOf ["foo","bar","baz"] ! "foobar"
                   |$.foo: minLength 3 ! 1""".stripMargin
    )

  test("parse"):
    assertEquals(
      obtained = Violations.parse(
        """$: type "string" ! "null"
          |$: oneOf ["foo","bar","baz"] ! "foobar"
          |$.foo: minLength 3 ! 1""".stripMargin
      ),
      expected = violations.asRight
    )
