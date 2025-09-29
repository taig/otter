package io.taig.otter.validation

import cats.Eq
import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*
import io.taig.data.Data

import java.util.regex.Pattern

final case class Violation[+C <: Constraint](constraint: C, actual: Data, hint: Option[String]) derives Eq:
  def modifyHint(f: Option[String] => Option[String]): Violation[C] = copy(hint = f(hint))
  def withHint(hint: Option[String]): Violation[C] = modifyHint(_ => hint)
  def withHint(hint: String): Violation[C] = withHint(hint = hint.some)
  def withoutHint: Violation[C] = withHint(hint = none)

  override def toString: String = hint match
    case Some(hint) => show"${constraint} ! ${actual} [$hint]"
    case None       => show"${constraint} ! ${actual}"

object Violation:
  def equal(reference: Data, actual: Data): Violation[Constraint.Equal] =
    Violation(Constraint.Equal(reference), actual, hint = none)

  def tpe(name: String, actual: Data, hint: Option[String]): Violation[Constraint.Type] =
    Violation(Constraint.Type(name), actual, hint)
  def tpe(name: String, actual: Data): Violation[Constraint.Type] = tpe(name, actual, hint = none)
  def tpe(name: String, actual: Data, hint: String): Violation[Constraint.Type] = tpe(name, actual, hint = hint.some)

  def required(hint: Option[String]): Violation[Constraint.Required] =
    Violation(Constraint.Required, actual = Data.Null, hint)
  def required(hint: String): Violation[Constraint.Required] = required(hint = hint.some)
  val required: Violation[Constraint.Required] = required(hint = none)

  def oneOf(values: List[Data], actual: Data): Violation[Constraint.OneOf] =
    Violation(Constraint.OneOf(values), actual, hint = none)

  def matches(pattern: Pattern, actual: Data): Violation[Constraint.Primitive.Text] =
    Violation(Constraint.Primitive.Text.Matches(pattern), actual, hint = none)
  def matches(expected: String, actual: Data): Violation[Constraint.Primitive.Text] =
    matches(pattern = Pattern.compile(Pattern.quote(expected)), actual)

  given [C <: Constraint]: Show[Violation[C]] = Show.fromToString
