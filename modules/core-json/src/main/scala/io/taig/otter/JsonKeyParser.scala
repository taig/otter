package io.taig.otter

import io.taig.otter.Json.Key

import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Primitive.String.Text
import io.taig.otter.Primitive.String.Modify

object JsonKeyParser extends Parser[Json.Key]:
  override def apply[A](codec: Json.Key[A], value: String): Validated[Violations, A] = codec match
    case Json.Key.Constant(self)  => ???
    case Json.Key.Primitive(self) => apply(codec = self, value)
    case Json.Key.Union(self)     => ???

  def apply[A](codec: Primitive.String[A], value: String): Validated[Violations, A] = codec match
    case Primitive.String.Parser(name, decode, _, minimum, maximum, matches, _) =>
      val length = value.length
      minimum.traverse(minimum =>
        Validated.cond(
          test = length >= minimum,
          (),
          Violations.rootNec(
            Violation(constraint = Constraint.Primitive.MinLength(reference = minimum), actual = length, hint = none)
          )
        )
      ) *> maximum.traverse(maximum =>
        Validated.cond(
          test = length <= maximum,
          (),
          Violations.rootNec(
            Violation(constraint = Constraint.Primitive.MinLength(reference = maximum), actual = length, hint = none)
          )
        )
      ) *> matches.traverse(pattern =>
        Validated.cond(
          test = pattern.matcher(value).matches(),
          (),
          Violations.rootNec(
            Violation(constraint = Constraint.Primitive.Matches(pattern), actual = length, hint = none)
          )
        )
      ) *> decode(value).toValidated.leftMap(hint =>
        Violations.rootNec(Violation(constraint = Constraint.Type(name), actual = value, hint = Some(hint)))
      )
    case Primitive.String.Text(minimum, maximum, matches, _) =>
      value.valid
    case Primitive.String.Modify(self, f, _) => apply(codec = self, value).map(f)
