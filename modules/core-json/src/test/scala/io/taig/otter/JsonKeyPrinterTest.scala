package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.JsonDsl.key.*

final class JsonKeyPrinterTest extends OtterSuite:
  val print = JsonKeyPrinter

  test("constant"):
    assertEq(
      obtained = print(constant("foo"), "bar"),
      expected = "foo"
    )

  test("primitive"):
    assertEq(
      obtained = print(string, "foobar"),
      expected = "foobar"
    )
    assertEq(
      obtained = print(string, ""),
      expected = ""
    )

  test("union"):
    val codec = branch("x", constant("foo")) :+ branch("y", constant("bar"))

    assertEq(
      obtained = print(codec, "foobar".asLeft),
      expected = "foo"
    )
    assertEq(
      obtained = print(codec, "foobar".asRight),
      expected = "bar"
    )
