package io.taig.otter

import munit.FunSuite
import io.taig.otter.JsonDsl.*

final class JsonZodRendererTest extends OtterSuite:
  test("constant"):
    assertEq(
      obtained = JsonZodRenderer(constant("foobar")),
      expected = """z.literal("foobar")"""
    )

  test("enumeration"):
    enum Animal:
      case Bird
      case Cat
      case Dog

    val codec: Json.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat => "cat"
      case Animal.Dog => "dog"
      
    assertEq(
      obtained = JsonZodRenderer(codec),
      expected = """z.enum(["bird", "cat", "dog"])"""
    )

  test("primitive"):
    assertEq(obtained = JsonZodRenderer(codec = string), expected = "z.string()")
    assertEq(obtained = JsonZodRenderer(codec = int), expected = "z.number()")
    assertEq(obtained = JsonZodRenderer(codec = long), expected = "z.number()")
    assertEq(obtained = JsonZodRenderer(codec = boolean), expected = "z.boolean()")
