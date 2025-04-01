package io.taig.otter

import io.taig.otter.TypescriptCodecs.*
import munit.FunSuite
import cats.data.State
import scala.collection.immutable.ListMap
import cats.syntax.all.*

final class ZodTypescriptRendererTest extends FunSuite:
  test("primitive"):
    assertEquals(
      obtained = ZodTypescriptRenderer(string).runA(ListMap.empty).value,
      expected = Expression.Inline("z.string()")
    )
    assertEquals(
      obtained = ZodTypescriptRenderer(int).runA(ListMap.empty).value,
      expected = Expression.Inline("z.number()")
    )
    assertEquals(
      obtained = ZodTypescriptRenderer(float).runA(ListMap.empty).value,
      expected = Expression.Inline("z.number()")
    )
    assertEquals(
      obtained = ZodTypescriptRenderer(boolean).runA(ListMap.empty).value,
      expected = Expression.Inline("z.boolean()")
    )

  // test("record"):
  //   val codec = field("foo", string) :* field("bar", int)

  //   assertEquals(
  //     obtained = ZodCodecPrinter.print(codec).runA(ListMap.empty).value,
  //     expected = Expression.Value("""z.object({
  //                                   |  "foo": z.string(),
  //                                   |  "bar": z.number()
  //                                   |})""".stripMargin)
  //   )

  // test("record: nested name (without namespace)"):
  //   val foo = string.name("Foo")
  //   val codec = field("foo", foo) :* field("bar", int)

  //   assertEquals(
  //     obtained = ZodCodecPrinter.print(codec).runA(ListMap.empty).value,
  //     expected = Expression.Value("""z.object({
  //                                   |  "foo": Foo,
  //                                   |  "bar": z.number()
  //                                   |})""".stripMargin)
  //   )

  // test("record: nested name (with namespace)"):
  //   val foo = string.namespace("x").name("Foo")
  //   val codec = field("foo", foo) :* field("bar", int)

  //   val (expressions, obtained) = ZodCodecPrinter.print(codec).run(ListMap.empty).value

  //   assertEquals(
  //     obtained,
  //     expected = Expression.Value("""z.object({
  //                                   |  "foo": x.Foo,
  //                                   |  "bar": z.number()
  //                                   |})""".stripMargin)
  //   )

  //   assertEquals(
  //     obtained = expressions,
  //     expected = ListMap(
  //       Expression.Reference(namespace = "x".some, value = "Foo") -> "z.string()"
  //     )
  //   )

  // test("override"):
  //   val value = "z.string().datetime().transform((value) => new Date(value))"
  //   val codec = string.typescript(value)

  //   assertEquals(
  //     obtained = ZodCodecPrinter.print(codec).runA(ListMap.empty).value,
  //     expected = Expression.Value(value)
  //   )

  // test("override: name"):
  //   val value = "z.string().datetime().transform((value) => new Date(value))"
  //   val instant = string.typescript(value).name("Instant")
  //   val codec = field("foo", instant).toRecord

  //   assertEquals(
  //     obtained = ZodCodecPrinter.print(codec).runA(ListMap.empty).value,
  //     expected = Expression.Value("""z.object({
  //                                   |  "foo": Instant
  //                                   |})""".stripMargin)
  //   )
