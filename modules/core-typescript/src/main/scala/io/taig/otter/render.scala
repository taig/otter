package io.taig.otter

import cats.data.NonEmptyList

import scala.util.chaining.*

/* Printing [[Typescript]].
 *
 * There is no measuring pass and no layout search. What decides whether something breaks is whether a child already
 * broke, and how long it would be on one line of its own. Neither of those needs to know how deep the result will end
 * up being indented, which is what keeps every function here total, independent and cheap.
 */

private val Indent: String = "  "

/* How long a thing may be before it is worth breaking. Not a column limit: the indentation it will be printed at is not
 * known here, so a nested value can still run past it. It is only there so that a short call does not break for the
 * sake of having more than one argument. */
private val Width: Int = 88

private val isMultiline: String => Boolean = _.linesIterator.drop(1).hasNext

/* An object literal is already delimited, so breaking around it would only add a line that says nothing. */
private val isBraced: String => Boolean = value => value.startsWith("{") && value.endsWith("}")

private val indent: Any => String = _.toString.linesIterator.map(Indent + _).mkString("\n")

/* Indents everything but the first line, for a value that begins where something else left off. */
private val align: Any => String = _.toString.linesIterator.zipWithIndex
  .map:
    case (line, 0) => line
    case (line, _) => Indent + line
  .mkString("\n")

private val renderTypescriptExpression: Typescript.Expression => String =
  case Typescript.Expression.Array(Nil)                  => "[]"
  case Typescript.Expression.Array(element :: Nil)       => s"[$element]"
  case Typescript.Expression.Array(elements)             => elements.map(indent).mkString("[\n", ",\n", "\n]")
  case Typescript.Expression.Arrow(arguments, body)      => s"(${arguments.mkString(", ")}) => $body"
  case Typescript.Expression.Call(name, Nil)             => s"$name()"
  case Typescript.Expression.Call(name, argument :: Nil) =>
    argument.render.pipe:
      case argument if isMultiline(argument) && !isBraced(argument) =>
        s"""$name(
           |${indent(argument)}
           |)""".stripMargin
      case argument => s"$name($argument)"
  case Typescript.Expression.Call(name, arguments) =>
    s"$name(${arguments.mkString(", ")})".pipe:
      case inlined if !isMultiline(inlined) && inlined.length <= Width => inlined
      case _                                                           =>
        s"""$name(
           |${arguments.map(indent).mkString(",\n")}
           |)""".stripMargin
  case Typescript.Expression.Equal(left, right)           => s"$left == $right"
  case Typescript.Expression.Literal.Boolean(value)       => String.valueOf(value)
  case Typescript.Expression.Literal.Number(value)        => value.toPlainString
  case Typescript.Expression.Literal.String(value)        => s"\"$value\""
  case Typescript.Expression.Member(namespace, property)  => s"$namespace.$property"
  case Typescript.Expression.Object(Nil)                  => "{}"
  case Typescript.Expression.Object((name, value) :: Nil) =>
    value.render.pipe:
      case value if isMultiline(value) =>
        s"""{
           |${indent(s"\"$name\": $value")}
           |}""".stripMargin
      case value => s"{ \"$name\": $value }"
  case Typescript.Expression.Object(fields) =>
    fields.map((name, value) => s"\"$name\": $value").map(indent).mkString("{\n", ",\n", "\n}")
  case Typescript.Expression.Pipe(self, arguments) =>
    (self.render :: arguments.toList.map(_.render)).pipe: rendered =>
      if rendered.exists(isMultiline)
      then s"""${self.render}.pipe(
              |${arguments.toList.map(indent).mkString(",\n")}
              |)""".stripMargin
      else s"$self.pipe(${arguments.toList.mkString(", ")})"
  case Typescript.Expression.Symbol(name)                       => name
  case Typescript.Expression.Ternary(condition, valid, invalid) => s"$condition ? $valid : $invalid"
  case Typescript.Expression.TripleEqual(left, right)           => s"$left === $right"

private val renderTypescriptStatement: Typescript.Statement => String =
  case Typescript.Statement.Block(statements)    => statements.map(indent).mkString("{\n", "\n", "\n}")
  case Typescript.Statement.Evaluate(expression) => s"$expression;"
  case Typescript.Statement.Declaration.Constant(true, name, None, value)       => s"export const $name = $value;"
  case Typescript.Statement.Declaration.Constant(false, name, None, value)      => s"const $name = $value;"
  case Typescript.Statement.Declaration.Constant(true, name, Some(tpe), value)  => s"export const $name: $tpe = $value;"
  case Typescript.Statement.Declaration.Constant(false, name, Some(tpe), value) => s"const $name: $tpe = $value;"
  case Typescript.Statement.Declaration.Variable(true, name, None, value)       => s"export let $name = $value;"
  case Typescript.Statement.Declaration.Variable(false, name, None, value)      => s"let $name = $value;"
  case Typescript.Statement.Declaration.Variable(true, name, Some(tpe), value)  => s"export let $name: $tpe = $value;"
  case Typescript.Statement.Declaration.Variable(false, name, Some(tpe), value) => s"let $name: $tpe = $value;"
  case Typescript.Statement.Declaration.Type(true, name, tpe)                   => s"export type $name = $tpe;"
  case Typescript.Statement.Declaration.Type(false, name, tpe)                  => s"type $name = $tpe;"

private val renderTypescriptType: Typescript.Type => String =
  case Typescript.Type.Literal.Boolean(value)      => String.valueOf(value)
  case Typescript.Type.Literal.Number(value)       => value.toPlainString
  case Typescript.Type.Literal.String(value)       => s"\"$value\""
  case Typescript.Type.Member(namespace, property) => s"$namespace.$property"
  case Typescript.Type.Null                        => "null"
  case Typescript.Type.Object(Nil)                 => "{}"
  case Typescript.Type.Object(field :: Nil)        =>
    renderTypescriptTypeField(field).pipe:
      case rendered if isMultiline(rendered) => s"{\n${indent(rendered + ";")}\n}"
      case rendered                          => s"{ $rendered }"
  case Typescript.Type.Object(fields) =>
    fields.map(renderTypescriptTypeField).map(_ + ";").map(indent).mkString("{\n", "\n", "\n}")
  case Typescript.Type.Symbol(name, Nil)        => name
  case Typescript.Type.Symbol(name, parameters) => s"$name<${parameters.mkString(", ")}>"
  case Typescript.Type.Tuple(Nil)               => "[]"
  case Typescript.Type.Tuple(elements)          =>
    elements
      .map(_.render)
      .pipe: rendered =>
        if rendered.exists(isMultiline)
        then elements.map(indent).mkString("[\n", ",\n", "\n]")
        else rendered.mkString("[", ", ", "]")
  case Typescript.Type.TypeOf(expression) => s"typeof $expression"
  case Typescript.Type.Undefined          => "undefined"
  case Typescript.Type.Union(types)       =>
    types.toList
      .map(_.render)
      .pipe: rendered =>
        rendered
          .mkString(" | ")
          .pipe:
            case inlined if !isMultiline(inlined) && inlined.length <= Width => inlined
            case _ => rendered.map(align).map("| " + _).mkString("\n")

/* Without its separator, which only the object that holds it knows whether to add. */
private val renderTypescriptTypeField: Typescript.Type.Field => String =
  case Typescript.Type.Field(name, tpe, false) => s"\"$name\": ${align(tpe)}"
  case Typescript.Type.Field(name, tpe, true)  =>
    s"\"$name\"?: ${align(Typescript.Type.Union(NonEmptyList.of(tpe, Typescript.Type.Undefined)))}"
