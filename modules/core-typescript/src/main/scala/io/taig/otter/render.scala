package io.taig.otter

import scala.util.chaining.*

private val Indent: String = "  "

private val isMultiline: String => Boolean = _.linesIterator.drop(1).hasNext

private val isObject: String => Boolean = value => value.startsWith("{") && value.endsWith("}")

private val indent: String => String = _.linesIterator.map(Indent + _).mkString("\n")

private val align: String => String = _.linesIterator.zipWithIndex
  .map:
    case (line, 0) => line
    case (line, _) => Indent + line
  .mkString("\n")

private val renderTypescript: Typescript => String =
  case typescript: Typescript.Expression => renderTypescriptExpression(typescript)
  case typescript: Typescript.Statement  => renderTypescriptStatement(typescript)

private val renderTypescriptExpression: Typescript.Expression => String =
  case Typescript.Expression.Array(Nil)            => "[]"
  case Typescript.Expression.Array(element :: Nil) => s"[$element]"
  case Typescript.Expression.Array(elements)       =>
    elements.map(element => Indent + s"$element").mkString("[\n", ",\n", "\n]")
  case Typescript.Expression.Call(name, Nil)             => s"$name()"
  case Typescript.Expression.Call(name, argument :: Nil) =>
    renderTypescriptExpression(argument).pipe:
      case argument if isMultiline(argument) && !isObject(argument) =>
        s"""$name(
           |${indent(argument)}
           |)""".stripMargin
      case argument => s"$name($argument)"
  case Typescript.Expression.Call(name, arguments) =>
    s"""$name(
       |${arguments.map(renderTypescriptExpression).map(indent).mkString("\n,")}
       |)""".stripMargin
  case Typescript.Expression.Identifier(name)             => name
  case Typescript.Expression.Member(namespace, property)  => s"$namespace.${property}"
  case Typescript.Expression.Literal.Boolean(value)       => String.valueOf(value)
  case Typescript.Expression.Literal.Number(value)        => value.toPlainString
  case Typescript.Expression.Literal.String(value)        => s"\"$value\""
  case Typescript.Expression.Object(Nil)                  => "{}"
  case Typescript.Expression.Object((name, value) :: Nil) =>
    renderTypescriptExpression(value).pipe:
      case value if isMultiline(value) =>
        s"""{ 
           |${indent(s"\"$name\": $value")}
           |}""".stripMargin
      case value => s"{ \"$name\": $value }"
  case Typescript.Expression.Object(fields) =>
    fields
      .map((name, value) => Indent + s"\"$name\": ${align(renderTypescriptExpression(value))}")
      .mkString("{\n", ",\n", "\n}")

private val renderTypescriptStatement: Typescript.Statement => String =
  case Typescript.Statement.Declaration.Constant(name, value) => s"const $name = $value;"
  case Typescript.Statement.Declaration.Variable(name, value) => s"let $name = $value;"
