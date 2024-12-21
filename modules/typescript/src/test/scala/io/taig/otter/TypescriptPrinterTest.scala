package io.taig.otter

import io.taig.otter.Dsl.*
import cats.syntax.all.*

final class TypescriptPrinterTest extends OtterSuite:
  test("primitive"):
    assertEq(obtained = TypescriptPrinter(bigDecimal), expected = "z.number()")
    assertEq(obtained = TypescriptPrinter(bigInt), expected = "z.number()")
    assertEq(obtained = TypescriptPrinter(boolean), expected = "z.boolean()")
    assertEq(obtained = TypescriptPrinter(double), expected = "z.number()")
    assertEq(obtained = TypescriptPrinter(float), expected = "z.number()")
    assertEq(obtained = TypescriptPrinter(int), expected = "z.number()")
    assertEq(obtained = TypescriptPrinter(long), expected = "z.number()")
    assertEq(obtained = TypescriptPrinter(string), expected = "z.string()")
    assertEq(obtained = TypescriptPrinter(uuid), expected = "z.string()")

  test("enumeration"):
    enum Data:
      case A
      case B
      case C

    val codec: Enumeration[Data] = enumeration(string):
      case Data.A => "a"
      case Data.B => "b"
      case Data.C => "c"

    assertEq(obtained = TypescriptPrinter(codec), expected = """z.enum(["a", "b", "c"])""")

  test("record"):
    val codec = field("a", string) :* field("b", int) :* field("c", boolean)

    assertEq(
      obtained = TypescriptPrinter(codec),
      expected = """z.object({
                   |  "a": z.string(),
                   |  "b": z.number(),
                   |  "c": z.boolean()
                   |})""".stripMargin
    )

  test("collection"):
    val codec = collection.list(string)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.array(z.string())")

  test("collection: nonEmpty"):
    val codec = collection.nonEmptyList(int)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.array(z.number()).nonempty()")

  test("collection: minItems"):
    val codec = collection.list(int, minItems = 3.some)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.array(z.number()).nonempty().min(3)")

  test("collection: maxItems"):
    val codec = collection.list(int, maxItems = 10.some)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.array(z.number()).max(10)")

  test("collection: length"):
    val codec = collection.list(int, minItems = 10.some, maxItems = 10.some)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.array(z.number()).nonempty().length(10)")

  test("collection: uniqueItems"):
    val codec = collection.set(int)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.set(z.number())")

  test("collection: minItems & maxItems"):
    val codec = collection.nonEmptyList(int, minItems = 3.some, maxItems = 10.some)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.array(z.number()).nonempty().min(3).max(10)")

  test("dictionary"):
    val codec = dictionary.map(string, int)
    assertEq(obtained = TypescriptPrinter(codec), expected = "z.map(z.string(), z.number())")
