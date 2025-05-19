package io.taig.otter.codec

import io.taig.otter.OtterSuite
import io.taig.otter.component.KeyComponent.*
import scala.collection.immutable.ListMap
import io.taig.otter.Key

final class KeyZodRendererTest extends OtterSuite:
  test("primitive"):
    assertEq(
      obtained = KeyZodRenderer.render(string),
      expected = "z.string()"
    )

    assertEq(
      obtained = KeyZodRenderer.render(uuid),
      expected = "z.string()"
    )

  test("enumeration"):
    enum Animal:
      case Bird
      case Cat
      case Dog

    val schema: Key.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat  => "cat"
      case Animal.Dog  => "dog"

    assertEq(
      obtained = KeyZodRenderer.render(schema),
      expected = """z.enum(["bird", "cat", "dog"])"""
    )
