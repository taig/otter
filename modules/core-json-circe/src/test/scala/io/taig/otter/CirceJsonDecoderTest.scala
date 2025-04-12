package io.taig.otter

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.JsonDsl.*

final class CirceJsonDecoderTest extends OtterSuite:
  val decoder: Decoder[Json, CirceJson] = CirceJsonDecoder.leftMap(_.modifyViolations(_.withoutHint))

  test("constant"):
    assertEq(
      obtained = decoder(constant(string, "foobar"), CirceJson.fromString("foobar")),
      expected = "foobar".valid
    )
    assertEq(
      obtained = decoder(constant(string, "foobar"), CirceJson.fromString("foo")),
      expected = Violations.rootNec(Violation.equal(reference = "foobar", actual = "foo")).invalid
    )
    assertEq(
      obtained = decoder(constant(string, "foobar"), CirceJson.fromInt(1)),
      expected = Violations.rootNec(Violation.tpe(name = "string", actual = 1)).invalid
    )

  test("primitive"):
    assertEq(
      obtained = decoder(boolean, CirceJson.fromBoolean(true)),
      expected = true.valid
    )
    assertEq(
      obtained = decoder(boolean, CirceJson.fromString("foobar")),
      expected = Violations.rootNec(Violation.tpe(name = "boolean", actual = "foobar")).invalid
    )

    assertEq(
      obtained = decoder(int, CirceJson.fromInt(1)),
      expected = 1.valid
    )
    assertEq(
      obtained = decoder(int, CirceJson.fromString("foobar")),
      expected = Violations.rootNec(Violation.tpe(name = "int", actual = "foobar")).invalid
    )

    assertEq(
      obtained = decoder(string, CirceJson.fromString("foobar")),
      expected = "foobar".valid
    )
    assertEq(
      obtained = decoder(string, CirceJson.fromInt(1)),
      expected = Violations.rootNec(Violation.tpe(name = "string", actual = 1)).invalid
    )
