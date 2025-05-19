package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.Keys.*
import io.taig.otter.ZodKeys.*

import scala.collection.immutable.ListMap
import io.taig.otter.OtterSuite
import io.taig.otter.Json
import io.taig.otter.ZodKeys.*
import io.taig.otter.ZodState
import io.taig.otter.ZodExpression
import io.taig.otter.ZodConst

final class JsonZodRendererTest extends OtterSuite:
  val renderer = JsonZodRenderer

  test("name"):
    assertEq(
      obtained = renderer.render(string.metadata(name, "Foobar")).runA(ListMap.empty).value,
      expected = ZodExpression.Referenced(ZodConst(namespace = none, name = "Foobar"), value = """z.string()""")
    )

  test("namespace"):
    assertEq(
      obtained = renderer.render(string.metadata(name, "Foo").metadata(namespace, "Bar")).runA(ListMap.empty).value,
      expected = ZodExpression.Referenced(ZodConst(namespace = "Bar".some, name = "Foo"), value = """z.string()""")
    )

  test("override"):
    assertEq(
      obtained = renderer.render(string.metadata(zod, "z.foobar()")).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.foobar()""")
    )

  test("override: reference"):
    assertEq(
      obtained = renderer.render(string.metadata(zod, "z.foobar()").metadata(name, "Foo")).runA(ListMap.empty).value,
      expected = ZodExpression.Referenced(
        reference = ZodConst(namespace = none, name = "Foo"),
        value = """z.foobar()"""
      )
    )

  test("collection"):
    assertEq(
      obtained = renderer.render(collection.list(string)).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.array(z.string())""")
    )

  test("collection: reference"):
    val schema = string.metadata(name, "Foo")

    assertEq(
      obtained = renderer.render(collection.list(schema)).run(ListMap.empty).value,
      expected = (
        ListMap(
          (ZodConst(namespace = none, name = "Foo"), "z.string()")
        ),
        ZodExpression.Inline("""z.array(Foo)""")
      )
    )

  test("constant"):
    assertEq(
      obtained = renderer.render(constant("foobar")).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.literal("foobar")""")
    )

  test("constant: unrepresentable"):
    assertEq(
      obtained = renderer.render(field("foo", string).toRecord).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.object({ "foo": z.string() })""")
    )

  test("dictionary"):
    assertEq(
      obtained = renderer.render(dictionary.list(key = key.string, value = long)).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.record(z.string(), z.number())""")
    )

  test("enumeration"):
    enum Animal:
      case Bird
      case Cat
      case Dog

    val schema: Json.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat  => "cat"
      case Animal.Dog  => "dog"

    assertEq(
      obtained = renderer.render(schema).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.enum(["bird", "cat", "dog"])""")
    )

  test("nullable"):
    assertEq(
      obtained = renderer.render(string.nullable).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.nullable(z.string())""")
    )

    assertEq(
      obtained = renderer.render(string.nullable(default = "foobar")).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.nullable(z.string())""")
    )

  test("nullable: reference"):
    val schema = string.metadata(name, "Foo")

    assertEq(
      obtained = renderer.render(schema.nullable).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("""z.nullable(Foo)""")
    )

  test("primitive"):
    assertEq(
      obtained = renderer.render(string).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("z.string()")
    )
    assertEq(
      obtained = renderer.render(int).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("z.number()")
    )
    assertEq(
      obtained = renderer.render(long).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("z.number()")
    )
    assertEq(
      obtained = renderer.render(boolean).runA(ListMap.empty).value,
      expected = ZodExpression.Inline("z.boolean()")
    )

  test("record"):
    val schema = field("foo", string) :* field("bar", int)

    assertEq(
      obtained = renderer.render(schema).runA(ListMap.empty).value,
      expected = ZodExpression.Inline(
        """z.object({
          |  "foo": z.string(),
          |  "bar": z.number()
          |})""".stripMargin
      )
    )

  test("record: reference"):
    val foo = string.metadata(name, "Foo")
    val schema = field("foo", foo) :* field("bar", int)

    assertEq(
      obtained = renderer.render(schema).run(ListMap.empty).value,
      expected = (
        ListMap((ZodConst(namespace = none, name = "Foo"), "z.string()")),
        ZodExpression.Inline(
          """z.object({
            |  "foo": Foo,
            |  "bar": z.number()
            |})""".stripMargin
        )
      )
    )

  test("record: optional"):
    val schema = field("foo", string) :* field("bar", int).optional

    assertEq(
      obtained = renderer.render(schema).runA(ListMap.empty).value,
      expected = ZodExpression.Inline(
        """z.object({
          |  "foo": z.string(),
          |  "bar": z.optional(z.number())
          |})""".stripMargin
      )
    )

  test("tuple"):
    assertEq(
      obtained = renderer.render(string :* int).runA(ListMap.empty).value,
      expected = ZodExpression.Inline(
        """z.tuple([
          |  z.string(),
          |  z.number()
          |])""".stripMargin
      )
    )

  test("tuple: reference"):
    val foo = string.metadata(name, "Foo")
    val schema = foo :* int

    assertEq(
      obtained = renderer.render(schema).run(ListMap.empty).value,
      expected = (
        ListMap((ZodConst(namespace = none, name = "Foo"), "z.string()")),
        ZodExpression.Inline(
          """z.tuple([
            |  Foo,
            |  z.number()
            |])""".stripMargin
        )
      )
    )

  test("union"):
    assertEq(
      obtained = renderer.render(string :+ int).runA(ListMap.empty).value,
      expected = ZodExpression.Inline(
        """z.union([
          |  z.string(),
          |  z.number()
          |])""".stripMargin
      )
    )