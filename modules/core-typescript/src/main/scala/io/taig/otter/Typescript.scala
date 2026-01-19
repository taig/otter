package io.taig.otter

import scala.Boolean as SBoolean
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal

object Typescript:
  private val Indent: String = "  "

  sealed abstract class Expression extends Product, Serializable:
    final override def toString: String = render

    final def render: String = render(level = 0)

    private def render(level: Int): String = this match
      case Typescript.Expression.Array(elements)       => s"[${elements.map(_.render(level)).mkString(", ")}]"
      case Typescript.Expression.Call(name, arguments) => s"$name(${arguments.map(_.render(level)).mkString(", ")})"
      case Typescript.Expression.Identifier(name)      => name
      case Typescript.Expression.Member(namespace, property)  => s"$namespace.${property.render(level)}"
      case Typescript.Expression.Literal.Boolean(value)       => String.valueOf(value)
      case Typescript.Expression.Literal.Number(value)        => value.toPlainString
      case Typescript.Expression.Literal.String(value)        => s"\"$value\""
      case Typescript.Expression.Object(Nil)                  => "{}"
      case Typescript.Expression.Object((name, value) :: Nil) => s"{ \"$name\": ${value.render(level)} }"
      case Typescript.Expression.Object(fields)               =>
        fields
          .map((name, value) => (Indent * (level + 1)) + s"\"$name\": ${value.render(level)}")
          .mkString("{\n", ",\n", "\n}")

  object Expression:
    final case class Array(elements: List[Typescript.Expression]) extends Typescript.Expression

    final case class Call(name: String, arguments: List[Typescript.Expression]) extends Typescript.Expression

    final case class Identifier(name: String) extends Typescript.Expression

    sealed abstract class Literal extends Typescript.Expression

    object Literal:
      final case class Boolean(value: SBoolean) extends Typescript.Expression.Literal
      final case class Number(value: JBigDecimal) extends Typescript.Expression.Literal
      final case class String(value: JString) extends Typescript.Expression.Literal

    final case class Member(namespace: String, property: Typescript.Expression) extends Typescript.Expression

    final case class Object(fields: List[(String, Typescript.Expression)]) extends Typescript.Expression
