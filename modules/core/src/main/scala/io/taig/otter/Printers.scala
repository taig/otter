package io.taig.otter

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter.Data.given

private[otter] object Printers:
  def apply(step: Step): String = step match
    case Step.Field(field) => s".$field"
    case Step.Index(index) => s"[$index]"

  def apply(xpath: XPath): String = "$" + xpath.toChain.map(Printers.apply).mkString_("")

  def apply(constraint: Constraint): String = constraint match
    case Constraint.Equal(reference) => show"equal \"$reference\""
    case Constraint.Type(name)       => show"type \"$name\""
    case Constraint.OneOf(values) =>
      show"oneOf [${values.map(Data.Primitive.show.show).mkString_(",")}]"
    case Constraint.Collection.MaxItems(reference)                  => show"maxItem $reference"
    case Constraint.Collection.MinItems(reference)                  => show"minItem $reference"
    case Constraint.Collection.UniqueItems                          => "uniqueItems"
    case Constraint.Object.MaxProperties(reference)                 => show"maxProperties $reference"
    case Constraint.Object.MinProperties(reference)                 => show"minProperties $reference"
    case Constraint.Primitive.Matches(pattern)                      => show"matches \"${pattern.pattern()}\""
    case Constraint.Primitive.Maximum(Comparison(reference, true))  => show"lt $reference"
    case Constraint.Primitive.Maximum(Comparison(reference, false)) => show"lteq $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, true))  => show"gt $reference"
    case Constraint.Primitive.Minimum(Comparison(reference, false)) => show"gteq $reference"
    case Constraint.Primitive.MaxLength(reference)                  => show"maxLength $reference"
    case Constraint.Primitive.MinLength(reference)                  => show"minLength $reference"
    case Constraint.Primitive.Multiple(reference)                   => show"multiple $reference"

  def apply(violation: Violation): String = violation.hint match
    case Some(hint) => show"${violation.constraint} ! ${violation.actual} [$hint]"
    case None       => show"${violation.constraint} ! ${violation.actual}"

  def apply(violations: Indexed[NonEmptyChain[Violation]]): String =
    val path = violations.xpath.show
    violations.self.map(violation => show"$path: $violation").mkString_("\n")

  def apply(violations: Violations): NonEmptyList[String] = violations.toNel.map(Printers.apply)
