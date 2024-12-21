package io.taig.otter

import io.taig.otter.Dsl.*

final class TypescriptPrinterTest extends OtterSuite:
  test("primitive"):
    assertEq(obtained = TypescriptPrinter(bigDecimal), expected = "number")
    assertEq(obtained = TypescriptPrinter(bigInt), expected = "number")
    assertEq(obtained = TypescriptPrinter(boolean), expected = "boolean")
    assertEq(obtained = TypescriptPrinter(double), expected = "number")
    assertEq(obtained = TypescriptPrinter(float), expected = "number")
    assertEq(obtained = TypescriptPrinter(int), expected = "number")
    assertEq(obtained = TypescriptPrinter(long), expected = "number")
    assertEq(obtained = TypescriptPrinter(string), expected = "string")
    assertEq(obtained = TypescriptPrinter(uuid), expected = "string")

  test("enumeration"):
    enum Data:
      case A
      case B
      case C

    val codec: Enumeration[Data] = enumeration(string):
      case Data.A => "a"
      case Data.B => "b"
      case Data.C => "c"

    assertEq(obtained = TypescriptPrinter(codec), expected = """"a" | "b" | "c"""")
