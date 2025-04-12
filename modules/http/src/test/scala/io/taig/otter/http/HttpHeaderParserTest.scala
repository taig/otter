package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.OtterSuite
import io.taig.otter.Violations
import io.taig.otter.http.HttpHeaderDsl.*

final class HttpHeaderParserTest extends OtterSuite:
  test("dictionary: explode = false"):
    assertEq(
      obtained = HttpHeaderParser(explode = false)(dictionary.list(string, string), "foo,bar,x,y"),
      expected = List(("foo", "bar"), ("x", "y")).valid
    )

  test("dictionary: explode = false (escaped)"):
    assertEq(
      obtained = HttpHeaderParser(explode = false)(dictionary.list(string, string), "foo,bar\\,baz,x\\,y\\,z,abc"),
      expected = List(("foo", "bar,baz"), ("x,y,z", "abc")).valid
    )

  test("dictionary: explode = true"):
    assertEq(
      obtained = HttpHeaderParser(explode = true)(dictionary.list(string, string), "foo=bar,x=y"),
      expected = List(("foo", "bar"), ("x", "y")).valid
    )

  test("dictionary: explode = true (escaped)"):
    assertEq(
      obtained = HttpHeaderParser(explode = true)(dictionary.list(string, string), "foo=foo\\=bar,x=a\\,b\\,c"),
      expected = List(("foo", "foo=bar"), ("x", "a,b,c")).valid
    )

  test("primitive"):
    assertEq(
      obtained = HttpHeaderParser(explode = false)(string, "foobar"),
      expected = "foobar".valid
    )

  test("tuple"):
    assertEq(
      obtained = HttpHeaderParser(explode = false)(string :* string, "foo,bar"),
      expected = ("foo", "bar").valid
    )

  test("tuple: escaped"):
    assertEq(
      obtained = HttpHeaderParser(explode = false)(string :* string, "foo\\,bar,foobar"),
      expected = ("foo,bar", "foobar").valid
    )
