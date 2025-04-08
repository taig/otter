package io.taig.otter.http

import io.taig.otter.http.HttpHeaderDsl.*
import io.taig.otter.OtterSuite

final class HttpHeaderPrinterTest extends OtterSuite:
  test("constant"):
    assertEq(
      obtained = HttpHeaderPrinter(explode = false)(constant(string, "foo"), "bar"),
      expected = "foo"
    )

  test("primitive"):
    assertEq(
      obtained = HttpHeaderPrinter(explode = false)(string, "foobar"),
      expected = "foobar"
    )

  test("tuple"):
    assertEq(
      obtained = HttpHeaderPrinter(explode = false)(string :* string, ("foo", "bar")),
      expected = "foo,bar"
    )
