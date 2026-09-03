package io.taig.otter

import cats.data.NonEmptyList

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import scala.Boolean as SBoolean

/** A fragment of TypeScript source.
  *
  * Modelled in the three sorts the language itself distinguishes, because a generator that builds strings cannot tell
  * whether it needs a comma or a semicolon, a `:` or a `=`, and cannot indent what it has already flattened. Rendering
  * is [[io.taig.otter.render]]; nothing here knows how it will be printed.
  */
sealed abstract class Typescript extends Product, Serializable:
  final override def toString: String = render

  def render: JString

object Typescript:
  /** The [[Metadata.Namespace]] the TypeScript renderers read their attributes from, whatever the target library. */
  val Namespace: Metadata.Namespace = Metadata.Namespace("typescript")

  sealed abstract class Expression extends Typescript:
    final override def render: JString = renderTypescriptExpression(this)

  object Expression:
    final case class Array(elements: List[Typescript.Expression]) extends Typescript.Expression

    final case class Arrow(arguments: List[Typescript.Expression], body: Typescript) extends Typescript.Expression

    final case class Call(name: JString, arguments: List[Typescript.Expression]) extends Typescript.Expression

    final case class Equal(left: Typescript.Expression, right: Typescript.Expression) extends Typescript.Expression

    sealed abstract class Literal extends Typescript.Expression

    object Literal:
      final case class Boolean(value: SBoolean) extends Typescript.Expression.Literal
      final case class Number(value: JBigDecimal) extends Typescript.Expression.Literal
      final case class String(value: JString) extends Typescript.Expression.Literal

    final case class Member(namespace: JString, property: Typescript.Expression) extends Typescript.Expression

    final case class Object(fields: List[(JString, Typescript.Expression)]) extends Typescript.Expression

    /** `self.pipe(a, b)`.
      *
      * [[Member]] reaches a property of a namespace, which is a name; this reaches one of an expression, which is what
      * refining an already built schema needs.
      */
    final case class Pipe(self: Typescript.Expression, arguments: NonEmptyList[Typescript.Expression])
        extends Typescript.Expression

    final case class Symbol(name: JString) extends Typescript.Expression

    final case class Ternary(
        condition: Typescript.Expression,
        valid: Typescript.Expression,
        invalid: Typescript.Expression
    ) extends Typescript.Expression

    final case class TripleEqual(left: Typescript.Expression, right: Typescript.Expression)
        extends Typescript.Expression

  sealed abstract class Statement extends Typescript:
    final override def render: JString = renderTypescriptStatement(this)

  object Statement:
    final case class Block(statements: List[Typescript.Statement]) extends Typescript.Statement

    sealed abstract class Declaration extends Typescript.Statement

    object Declaration:
      final case class Constant(
          exported: SBoolean,
          name: JString,
          tpe: Option[Typescript.Type],
          value: Typescript.Expression
      ) extends Typescript.Statement.Declaration

      final case class Variable(
          exported: SBoolean,
          name: JString,
          tpe: Option[Typescript.Type],
          value: Typescript.Expression
      ) extends Typescript.Statement.Declaration

      final case class Type(exported: SBoolean, name: JString, tpe: Typescript.Type)
          extends Typescript.Statement.Declaration

    final case class Evaluate(expression: Typescript.Expression) extends Typescript.Statement

  sealed abstract class Type extends Typescript:
    final override def render: JString = renderTypescriptType(this)

  object Type:
    /** A member of an object type. `optional` renders the key as `"name"?:` and widens the type by `undefined`. */
    final case class Field(name: JString, tpe: Typescript.Type, optional: SBoolean)

    sealed abstract class Literal extends Typescript.Type

    object Literal:
      final case class Boolean(value: SBoolean) extends Typescript.Type.Literal
      final case class Number(value: JBigDecimal) extends Typescript.Type.Literal
      final case class String(value: JString) extends Typescript.Type.Literal

    final case class Member(namespace: JString, property: Typescript.Type) extends Typescript.Type

    case object Null extends Typescript.Type

    final case class Object(fields: List[Typescript.Type.Field]) extends Typescript.Type

    final case class Symbol(name: JString, parameters: List[Typescript.Type]) extends Typescript.Type

    /** `readonly T`, which is what a tuple or an array a schema produces is. Only tuples and arrays carry it: on an
      * object member `readonly` says nothing about assignability, and the members already say what they are.
      */
    final case class Readonly(self: Typescript.Type) extends Typescript.Type

    /** The `...T` that stands for the rest of a tuple's elements, where `T` is the array type they make up. */
    final case class Rest(self: Typescript.Type) extends Typescript.Type

    final case class Tuple(elements: List[Typescript.Type]) extends Typescript.Type

    final case class TypeOf(expression: Typescript.Expression) extends Typescript.Type

    case object Undefined extends Typescript.Type

    final case class Union(types: NonEmptyList[Typescript.Type]) extends Typescript.Type
