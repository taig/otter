package io.taig.otter.codec

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.codec.CirceJsonDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter
import io.taig.otter.OtterSuite
import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.Violations
import io.taig.otter.Violation

final class CirceJsonDecoderTest extends OtterSuite:
  val decoder: Decoder[Json, CirceJson] = CirceJsonDecoder.leftMap(_.modifyViolations(_.withoutHint))

  test("constant"):
    assertEq(
      obtained = decoder.decode(constant(string, "foobar"), CirceJson.fromString("foobar")),
      expected = ().valid
    )
    assertEq(
      obtained = decoder.decode(constant(string, "foobar"), CirceJson.fromString("foo")),
      expected = Violations.rootNec(Violation.equal(reference = "foobar", actual = "foo")).invalid
    )
    assertEq(
      obtained = decoder.decode(constant(string, "foobar"), CirceJson.fromInt(1)),
      expected = Violations.rootNec(Violation.tpe(name = "string", actual = 1)).invalid
    )

  test("primitive"):
    assertEq(
      obtained = decoder.decode(boolean, CirceJson.fromBoolean(true)),
      expected = true.valid
    )
    assertEq(
      obtained = decoder.decode(boolean, CirceJson.fromString("foobar")),
      expected = Violations.rootNec(Violation.tpe(name = "boolean", actual = "foobar")).invalid
    )

    assertEq(
      obtained = decoder.decode(int, CirceJson.fromInt(1)),
      expected = 1.valid
    )
    assertEq(
      obtained = decoder.decode(int, CirceJson.fromString("foobar")),
      expected = Violations.rootNec(Violation.tpe(name = "int", actual = "foobar")).invalid
    )

    assertEq(
      obtained = decoder.decode(string, CirceJson.fromString("foobar")),
      expected = "foobar".valid
    )
    assertEq(
      obtained = decoder.decode(string, CirceJson.fromInt(1)),
      expected = Violations.rootNec(Violation.tpe(name = "string", actual = 1)).invalid
    )
