package io.taig.otter

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*

private[otter] object Printers:
  def apply(step: Step): String = step match
    case Step.Field(field) => s".$field"
    case Step.Index(index) => s"[$index]"

  def apply(xpath: XPath): String = "$" + xpath.toChain.map(Printers.apply).mkString_("")

  def apply(constraint: Constraint): String = constraint match
    case Constraint.Type(name)                                      => s"type \"$name\""
    case Constraint.OneOf(values)                                   => s"oneOf [${values.toList.mkString(",")}]"
    case Constraint.Collection.MaxItems(reference)                  => s"maxItem $reference"
    case Constraint.Collection.MinItems(reference)                  => s"minItem $reference"
    case Constraint.Collection.UniqueItems                          => "uniqueItems"
    case Constraint.Object.MaxProperties(reference)                 => s"maxProperties $reference"
    case Constraint.Object.MinProperties(reference)                 => s"minProperties $reference"
    case Constraint.Primitive.Matches(pattern)                      => s"matches \"$pattern\""
    case Constraint.Primitive.Maximum(Comparison(reference, true))  => s"lt $reference"
    case Constraint.Primitive.Maximum(Comparison(reference, false)) => s"lteq $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, true))  => s"gt $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, false)) => s"gteq $reference"
    case Constraint.Primitive.MaxLength(reference)                  => s"maxLength $reference"
    case Constraint.Primitive.MinLength(reference)                  => s"minLength $reference"
    case Constraint.Primitive.Multiple(reference)                   => s"multiple $reference"

  def apply(data: Data, quoted: Boolean): String = data match
    case Data.Object(values) =>
      s"{${values.map { case (key, value) => s"\"$key\":${Printers(value, quoted)}" }.mkString(",")}}"
    case Data.Array(values)       => s"[${values.map(Printers(_, quoted)).mkString(",")}]"
    case Data.String(value)       => if quoted then s"\"$value\"" else value
    case Data.Boolean(value)      => String.valueOf(value)
    case Data.Number(value)       => String.valueOf(value)
    case Data.Nullable.Some(data) => apply(data, quoted)
    case Data.Nullable.None       => "null"

  def apply(violation: Violation): String = show"${violation.constraint} ! ${violation.actual}"

  def apply(violations: Indexed[NonEmptyChain[Violation]]): String =
    val path = violations.xpath.show
    violations.self.map(violation => show"$path: $violation").mkString_("\n")

  def apply(violations: Violations): NonEmptyList[String] = violations.toNel.map(Printers.apply)
