package io.taig.otter

import cats.data.NonEmptyList

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import scala.Boolean as SBoolean

sealed abstract class Typescript extends Product, Serializable:
  final override def toString: String = render

  def render: String

object Typescript:
  sealed abstract class Expression extends Typescript:
    final override def render: String = renderTypescriptExpression(this)

  object Expression:
    final case class Array(elements: List[Typescript.Expression]) extends Typescript.Expression

    final case class Arrow(arguments: List[Typescript.Expression], body: Typescript) extends Typescript.Expression

    final case class Call(name: String, arguments: List[Typescript.Expression]) extends Typescript.Expression

    final case class Equal(left: Typescript.Expression, right: Typescript.Expression) extends Typescript.Expression

    sealed abstract class Literal extends Typescript.Expression

    object Literal:
      final case class Boolean(value: SBoolean) extends Typescript.Expression.Literal
      final case class Number(value: JBigDecimal) extends Typescript.Expression.Literal
      final case class String(value: JString) extends Typescript.Expression.Literal

    final case class Member(namespace: String, property: Typescript.Expression) extends Typescript.Expression

    final case class Object(fields: List[(String, Typescript.Expression)]) extends Typescript.Expression

    final case class Symbol(name: String) extends Typescript.Expression

    final case class Ternary(
        condition: Typescript.Expression,
        valid: Typescript.Expression,
        invalid: Typescript.Expression
    ) extends Typescript.Expression

    final case class TripleEqual(left: Typescript.Expression, right: Typescript.Expression)
        extends Typescript.Expression

  sealed abstract class Statement extends Typescript:
    final override def render: String = renderTypescriptStatement(this)

  object Statement:
    final case class Block(statements: List[Typescript.Statement]) extends Typescript.Statement

    sealed abstract class Declaration extends Typescript.Statement

    object Declaration:
      final case class Constant(
          exported: Boolean,
          name: String,
          tpe: Option[Typescript.Type],
          value: Typescript.Expression
      ) extends Typescript.Statement.Declaration

      final case class Variable(
          exported: Boolean,
          name: String,
          tpe: Option[Typescript.Type],
          value: Typescript.Expression
      ) extends Typescript.Statement.Declaration

      final case class Type(exported: Boolean, name: String, tpe: Typescript.Type)
          extends Typescript.Statement.Declaration

    final case class Evaluate(expression: Typescript.Expression) extends Typescript.Statement

  sealed abstract class Type extends Typescript:
    final override def render: String = renderTypescriptType(this)

  object Type:
    final case class Field(name: String, tpe: Typescript.Type, optional: Boolean)

    sealed abstract class Literal extends Typescript.Type

    object Literal:
      final case class Boolean(value: SBoolean) extends Typescript.Type.Literal
      final case class Number(value: JBigDecimal) extends Typescript.Type.Literal
      final case class String(value: JString) extends Typescript.Type.Literal

    final case class Member(namespace: String, property: Typescript.Type) extends Typescript.Type

    case object Null extends Typescript.Type

    final case class Object(fields: List[Typescript.Type.Field]) extends Typescript.Type

    final case class Symbol(name: String, parameters: List[Typescript.Type]) extends Typescript.Type

    final case class Tuple(elements: List[Typescript.Type]) extends Typescript.Type

    final case class TypeOf(expression: Typescript.Expression) extends Typescript.Type

    case object Undefined extends Typescript.Type

    final case class Union(types: NonEmptyList[Typescript.Type]) extends Typescript.Type
