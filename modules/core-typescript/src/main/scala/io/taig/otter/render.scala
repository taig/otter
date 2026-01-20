package io.taig.otter

private val Indent: String = "  "

private val renderTypescript: Typescript => String =
  case typescript: Typescript.Expression => renderTypescriptExpression(typescript)
  case typescript: Typescript.Statement  => renderTypescriptStatement(typescript)

private val renderTypescriptExpression: Typescript.Expression => String =
  case Typescript.Expression.Array(Nil)            => "[]"
  case Typescript.Expression.Array(element :: Nil) => s"[$element]"
  case Typescript.Expression.Array(elements)       =>
    elements.map(element => Indent + s"$element").mkString("[\n", ",\n", "\n]")
  case Typescript.Expression.Call(name, arguments)        => s"$name(${arguments.mkString(", ")})"
  case Typescript.Expression.Identifier(name)             => name
  case Typescript.Expression.Member(namespace, property)  => s"$namespace.${property}"
  case Typescript.Expression.Literal.Boolean(value)       => String.valueOf(value)
  case Typescript.Expression.Literal.Number(value)        => value.toPlainString
  case Typescript.Expression.Literal.String(value)        => s"\"$value\""
  case Typescript.Expression.Object(Nil)                  => "{}"
  case Typescript.Expression.Object((name, value) :: Nil) => s"{ \"$name\": $value }"
  case Typescript.Expression.Object(fields)               =>
    fields
      .map((name, value) => Indent + s"\"$name\": $value")
      .mkString("{\n", ",\n", "\n}")

private val renderTypescriptStatement: Typescript.Statement => String =
  case Typescript.Statement.Declaration.Constant(name, value) => s"const $name = $value;"
  case Typescript.Statement.Declaration.Variable(name, value) => s"let $name = $value;"
