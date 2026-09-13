package io.taig.otter.codec

import io.taig.otter.Typescript

/** Tests the decimal coefficient before Number can round a fractional or out-of-range Int into a valid one. No
  * exponentiation or padding: even an enormous exponent takes work proportional only to the input length.
  */
private[otter] object TypescriptIntString:
  private def symbol(name: String): Typescript.Expression = Typescript.Expression.Symbol(name)
  private def text(value: String): Typescript.Expression = Typescript.Expression.Literal.String(value)
  private def number(value: Int): Typescript.Expression =
    Typescript.Expression.Literal.Number(java.math.BigDecimal.valueOf(value))
  private def binary(
      left: Typescript.Expression,
      op: Typescript.Expression.Operator,
      right: Typescript.Expression
  ): Typescript.Expression =
    Typescript.Expression.Binary(left, op, right)
  private def length(name: String): Typescript.Expression = Typescript.Expression.Member(name, symbol("length"))
  private def item(name: String, index: Int, fallback: String): Typescript.Expression =
    binary(
      Typescript.Expression.Index(symbol(name), number(index)),
      Typescript.Expression.Operator.Otherwise,
      text(fallback)
    )
  private def invoke(self: Typescript.Expression, name: String, args: Typescript.Expression*): Typescript.Expression =
    Typescript.Expression.Invoke(self, name, args.toList)
  private def replace(self: Typescript.Expression, pattern: String): Typescript.Expression =
    invoke(self, "replace", Typescript.Expression.Call("RegExp", List(text(pattern), text("g"))), text(""))
  private def constant(name: String, value: Typescript.Expression): Typescript.Statement =
    Typescript.Statement.Declaration.Constant(exported = false, name, None, value)

  val predicate: Typescript.Expression =
    import Typescript.Expression.Operator.*
    val scale = binary(
      binary(
        binary(Typescript.Expression.Call("Number", List(item("parts", 1, "0"))), Subtract, length("fraction")),
        Add,
        length("coefficient")
      ),
      Subtract,
      length("significant")
    )
    val valid = List(
      binary(symbol("scale"), AtLeast, number(0)),
      binary(binary(length("significant"), Add, symbol("scale")), AtMost, number(10)),
      binary(symbol("decoded"), AtLeast, number(Int.MinValue)),
      binary(symbol("decoded"), AtMost, number(Int.MaxValue))
    ).reduceLeft((left, right) => binary(left, And, right))
    Typescript.Expression.Arrow(
      List(symbol("value")),
      Typescript.Statement.Block(
        List(
          constant("parts", invoke(invoke(symbol("value"), "toLowerCase"), "split", text("e"))),
          constant("decimal", invoke(item("parts", 0, ""), "split", text("."))),
          constant("fraction", item("decimal", 1, "")),
          constant(
            "coefficient",
            replace(replace(binary(item("decimal", 0, ""), Add, symbol("fraction")), "^-"), "^0+")
          ),
          constant("significant", replace(symbol("coefficient"), "0+$")),
          constant("scale", scale),
          constant("decoded", Typescript.Expression.Call("Number", List(symbol("value")))),
          Typescript.Statement.Return(
            binary(Typescript.Expression.TripleEqual(symbol("significant"), text("")), Or, valid)
          )
        )
      )
    )
