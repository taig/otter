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

final class CirceJsonEncoderTest extends OtterSuite:
  val encoder: Encoder[Json, CirceJson] = CirceJsonEncoder

  test("collection"):
    assertEq(
      obtained = encoder.encode(collection.list(int), List(1, 2, 3)),
      expected = CirceJson.arr(
        CirceJson.fromInt(1),
        CirceJson.fromInt(2),
        CirceJson.fromInt(3)
      )
    )

  test("constant"):
    assertEq(
      obtained = encoder.encode(constant("foobar"), ""),
      expected = CirceJson.fromString("foobar")
    )

  test("dictionary"):
    assertEq(
      obtained = encoder.encode(dictionary.list(key = key.string, value = long), List(("foo", 1L), ("bar", 2L))),
      expected = CirceJson.fromFields(List(("foo", CirceJson.fromLong(1)), ("bar", CirceJson.fromLong(2))))
    )

  test("enumeration"):
    enum Animal:
      case Bird
      case Cat
      case Dog

    val codec: Json.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat  => "cat"
      case Animal.Dog  => "dog"

    assertEq(
      obtained = encoder.encode(codec, Animal.Bird),
      expected = CirceJson.fromString("bird")
    )
    assertEq(
      obtained = encoder.encode(codec, Animal.Cat),
      expected = CirceJson.fromString("cat")
    )
    assertEq(
      obtained = encoder.encode(codec, Animal.Dog),
      expected = CirceJson.fromString("dog")
    )

  test("optional"):
    assertEq(
      obtained = encoder.encode(string.nullable, "foobar".some),
      expected = CirceJson.fromString("foobar")
    )
    assertEq(
      obtained = encoder.encode(string.nullable, none),
      expected = CirceJson.Null
    )
    assertEq(
      obtained = encoder.encode(string.nullable("fallback"), "foobar"),
      expected = CirceJson.fromString("foobar")
    )

  test("primitive"):
    assertEq(
      obtained = encoder.encode(string, "foobar"),
      expected = CirceJson.fromString("foobar")
    )
    assertEq(
      obtained = encoder.encode(int, 1),
      expected = CirceJson.fromInt(1)
    )
    assertEq(
      obtained = encoder.encode(long, 1L),
      expected = CirceJson.fromLong(1L)
    )
    assertEq(
      obtained = encoder.encode(boolean, true),
      expected = CirceJson.fromBoolean(true)
    )

  test("record"):
    val codec = field("foo", string) :* field("bar", int)

    assertEq(
      obtained = encoder.encode(codec, ("foobar", 1)),
      expected = CirceJson.fromFields(
        List(("foo", CirceJson.fromString("foobar")), ("bar", CirceJson.fromInt(1)))
      )
    )

  test("record: optional"):
    val codec = field("foo", string) :* field("bar", int).optional

    assertEq(
      obtained = encoder.encode(codec, ("foobar", 1.some)),
      expected = CirceJson.fromFields(
        List(("foo", CirceJson.fromString("foobar")), ("bar", CirceJson.fromInt(1)))
      )
    )

    assertEq(
      obtained = encoder.encode(codec, ("foobar", none)),
      expected = CirceJson.fromFields(List(("foo", CirceJson.fromString("foobar"))))
    )

  test("tuple"):
    val codec = string :* int

    assertEq(
      obtained = encoder.encode(codec, ("foobar", 42)),
      expected = CirceJson.fromValues(
        List(CirceJson.fromString("foobar"), CirceJson.fromInt(42))
      )
    )

  test("union: untagged"):
    val codec = branch("foo", string) | branch("bar", int)

    assertEq(
      obtained = encoder.encode(codec, "foobar"),
      expected = CirceJson.fromString("foobar")
    )
    assertEq(
      obtained = encoder.encode(codec, 1),
      expected = CirceJson.fromInt(1)
    )

  test("union: taggged (keyed)"):
    val codec = (branch("foo", string) | branch("bar", int)).keyed

    assertEq(
      obtained = encoder.encode(codec, "foobar"),
      expected = CirceJson.fromFields(List(("foo", CirceJson.fromString("foobar"))))
    )
    assertEq(
      obtained = encoder.encode(codec, 1),
      expected = CirceJson.fromFields(List(("bar", CirceJson.fromInt(1))))
    )

  test("union: taggged (merged)"):
    val codec = (branch("foo", field("x", string)) | branch("bar", field("y", int))).merged

    assertEq(
      obtained = encoder.encode(codec, "foobar"),
      expected = CirceJson.fromFields(
        List(("type", CirceJson.fromString("foo")), ("x", CirceJson.fromString("foobar")))
      )
    )
    assertEq(
      obtained = encoder.encode(codec, 1),
      expected = CirceJson.fromFields(
        List(("type", CirceJson.fromString("bar")), ("y", CirceJson.fromInt(1)))
      )
    )

  test("union: taggged (explicit)"):
    val codec = (branch("foo", string) | branch("bar", int)).explicit

    assertEq(
      obtained = encoder(codec, "foobar"),
      expected = CirceJson.fromFields(
        List(("type", CirceJson.fromString("foo")), ("value", CirceJson.fromString("foobar")))
      )
    )
    assertEq(
      obtained = encoder(codec, 1),
      expected = CirceJson.fromFields(
        List(("type", CirceJson.fromString("bar")), ("value", CirceJson.fromInt(1)))
      )
    )
