package io.taig.otter

import munit.FunSuite
import cats.data.State
import scala.collection.immutable.ListMap
import io.taig.otter.Dsl.*
import cats.syntax.all.*
import io.taig.otter.Parsers.data.number

final class ZodCodecPrinterTest extends FunSuite:
  private val printer: CodecPrinter[State[ListMap[Expression.Reference, String], *]] =
    ZodCodecPrinter()

  test("primitive"):
    assertEquals(
      obtained = printer.print(string).runA(ListMap.empty).value,
      expected = "z.string()"
    )
    assertEquals(
      obtained = printer.print(int).runA(ListMap.empty).value,
      expected = "z.number()"
    )
    assertEquals(
      obtained = printer.print(float).runA(ListMap.empty).value,
      expected = "z.number()"
    )
    assertEquals(
      obtained = printer.print(boolean).runA(ListMap.empty).value,
      expected = "z.boolean()"
    )

  test("record"):
    val codec = field("foo", string) :* field("bar", int)

    assertEquals(
      obtained = printer.print(codec).runA(ListMap.empty).value,
      expected = """z.object({
                   |  "foo": z.string(),
                   |  "bar": z.number()
                   |})""".stripMargin
    )

  test("record: nested name (without namespace)"):
    val foo = string.name("Foo")
    val codec = field("foo", foo) :* field("bar", int)

    assertEquals(
      obtained = printer.print(codec).runA(ListMap.empty).value,
      expected = """z.object({
                   |  "foo": Foo,
                   |  "bar": z.number()
                   |})""".stripMargin
    )

  test("record: nested name (with namespace)"):
    val foo = string.namespace("x").name("Foo")
    val codec = field("foo", foo) :* field("bar", int)

    val (expressions, obtained) = printer.print(codec).run(ListMap.empty).value

    assertEquals(
      obtained,
      expected = """z.object({
                   |  "foo": x.Foo,
                   |  "bar": z.number()
                   |})""".stripMargin
    )

    assertEquals(
      obtained = expressions,
      expected = ListMap(
        Expression.Reference(namespace = "x".some, value = "Foo") -> "z.string()"
      )
    )

  test("override"):
    val value = "z.string().datetime().transform((value) => new Date(value))"
    val codec = string.typescript(value)

    assertEquals(obtained = printer.print(codec).runA(ListMap.empty).value, expected = value)

  test("override: name"):
    val value = "z.string().datetime().transform((value) => new Date(value))"
    val instant = string.typescript(value).name("Instant")
    val codec = field("foo", instant).toRecord

    assertEquals(
      obtained = printer.print(codec).runA(ListMap.empty).value,
      expected = """z.object({
                   |  "foo": Instant
                   |})""".stripMargin
    )
