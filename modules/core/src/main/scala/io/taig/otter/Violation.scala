package io.taig.otter

import cats.Eq
import cats.Show
import cats.derived.strict.*
import cats.syntax.all.*

import java.util.regex.Pattern

final case class Violation(constraint: Constraint, actual: Data, hint: Option[String]) derives Eq:
  def modifyHint(f: Option[String] => Option[String]): Violation = copy(hint = f(hint))
  def withHint(hint: Option[String]): Violation = modifyHint(_ => hint)
  def withHint(hint: String): Violation = withHint(hint = hint.some)
  def withoutHint: Violation = withHint(hint = none)

  override def toString: String = hint match
    case Some(hint) => show"${constraint} ! ${actual} [$hint]"
    case None       => show"${constraint} ! ${actual}"

object Violation:
  def equal(reference: Data, actual: Data): Violation =
    Violation(Constraint.Equal(reference), actual, hint = none)

  def tpe(name: String, actual: Data, hint: Option[String]): Violation =
    Violation(Constraint.Type(name), actual, hint)
  def tpe(name: String, actual: Data): Violation = tpe(name, actual, hint = none)
  def tpe(name: String, actual: Data, hint: String): Violation = tpe(name, actual, hint = hint.some)

  def required(hint: Option[String]): Violation = Violation(Constraint.Required, actual = Data.Null, hint)
  def required(hint: String): Violation = required(hint = hint.some)
  val required: Violation = required(hint = none)

  def oneOf(values: List[Data], actual: Data): Violation =
    Violation(Constraint.OneOf(values), actual, hint = none)

  def matches(pattern: Pattern, actual: Data): Violation =
    Violation(Constraint.Primitive.String.Matches(pattern), actual, hint = none)
  def matches(expected: String, actual: Data): Violation =
    matches(pattern = Pattern.compile(Pattern.quote(expected)), actual)

  given Show[Violation] = Show.fromToString
