package io.taig.otter.http

import io.taig.otter.OtterSuite
import io.taig.otter.http.HttpDsl.*

final class PathPrinterTest extends OtterSuite:
  val print = PathPrinter

  test("segment: empty"):
    assertEq(obtained = print(path.empty, ()), expected = "/")

  test("segment: static"):
    assertEq(obtained = print(segment(name = "foobar").toPath, ()), expected = "/foobar")

  test("segment: parameter"):
    assertEq(obtained = print(segment(name = "foo", segment.string).toPath, "bar"), expected = "/bar")

  test("path"):
    assertEq(obtained = print(path.empty, ()), expected = "/")
