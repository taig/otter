package io.taig.otter

import scala.util.chaining.*

private val Indent: String = "  "

private val isMultiline: String => Boolean = _.linesIterator.drop(1).hasNext

private val isObject: String => Boolean = value => value.startsWith("{") && value.endsWith("}")

private val indent: Any => String = _.toString.linesIterator.map(Indent + _).mkString("\n")

private val align: Any => String = _.toString.linesIterator.zipWithIndex
  .map:
    case (line, 0) => line
    case (line, _) => Indent + line
  .mkString("\n")

private val renderTypescriptExpression: Typescript.Expression => String =
  case Typescript.Expression.Array(Nil)                  => "[]"
  case Typescript.Expression.Array(element :: Nil)       => s"[$element]"
  case Typescript.Expression.Array(elements)             => elements.map(indent).mkString("[\n", ",\n", "\n]")
  case Typescript.Expression.Arrow(body)                 => s"() => $body"
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
       |${arguments.map(indent).mkString("\n,")}
       |)""".stripMargin
  case Typescript.Expression.Symbol(name)                 => name
  case Typescript.Expression.Member(namespace, property)  => s"$namespace.${property}"
  case Typescript.Expression.Literal.Boolean(value)       => String.valueOf(value)
  case Typescript.Expression.Literal.Number(value)        => value.toPlainString
  case Typescript.Expression.Literal.String(value)        => s"\"$value\""
  case Typescript.Expression.Object(Nil)                  => "{}"
  case Typescript.Expression.Object((name, value) :: Nil) =>
    s"$value".pipe:
      case value if isMultiline(value) =>
        s"""{ 
           |${indent(s"\"$name\": $value")}
           |}""".stripMargin
      case value => s"{ \"$name\": $value }"
  case Typescript.Expression.Object(fields) =>
    fields
      .map((name, value) => s"\"$name\": $value")
      .map(indent)
      .mkString("{\n", ",\n", "\n}")

private val renderTypescriptStatement: Typescript.Statement => String =
  case Typescript.Statement.Block(statements)    => statements.map(indent).mkString("{\n", "\n", "\n}")
  case Typescript.Statement.Evaluate(expression) => s"$expression;"
  case Typescript.Statement.Declaration.Constant(name, None, value)      => s"const $name = $value;"
  case Typescript.Statement.Declaration.Constant(name, Some(tpe), value) => s"const $name: $tpe = $value;"
  case Typescript.Statement.Declaration.Variable(name, None, value)      => s"let $name = $value;"
  case Typescript.Statement.Declaration.Variable(name, Some(tpe), value) => s"let $name: $tpe = $value;"
  case Typescript.Statement.Declaration.Type(name, tpe)                  => s"type $name = $tpe;"

private val renderTypescriptType: Typescript.Type => String =
  case Typescript.Type.Literal.Boolean(value)       => String.valueOf(value)
  case Typescript.Type.Literal.Number(value)        => value.toPlainString
  case Typescript.Type.Literal.String(value)        => s"\"$value\""
  case Typescript.Type.Member(namespace, property)  => s"$namespace.$property"
  case Typescript.Type.Null                         => "null"
  case Typescript.Type.Object(Nil)                  => "{}"
  case Typescript.Type.Object((name, value) :: Nil) => s"{ \"$name\": $value }"
  case Typescript.Type.Object(fields)               =>
    fields
      .map((name, value) => s"\"$name\": $value;")
      .map(indent)
      .mkString("{\n", "\n", "\n}")
  case Typescript.Type.Symbol(name, Nil)        => name
  case Typescript.Type.Symbol(name, parameters) => s"$name<${parameters.mkString(", ")}>"
  case Typescript.Type.Tuple(elements)          => elements.mkString("[\n", ",", "\n]")
  case Typescript.Type.TypeOf(expression)       => s"typeof $expression"
  case Typescript.Type.Undefined                => "undefined"
  case Typescript.Type.Union(types)             => types.map(_.toString).toList.mkString(" | ")
