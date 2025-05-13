package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constant.Modify
import io.taig.otter.Constant.Root
import io.taig.otter.Json.Key

import java.util.regex.Pattern

object JsonKeyParser extends Parser[Json.Key]:
  override def apply[A](codec: Json.Key[A], value: String): Validated[Violations, A] = codec match
    case Json.Key.Constant(self)    => apply(codec = self, value)
    case Json.Key.Enumeration(self) => apply(codec = self, value)
    case Json.Key.Primitive(self)   => apply(codec = self, value)
    case Json.Key.Union(self)       => apply(codec = self, value)

  def apply[A](codec: Constant[Json.Key, A], value: String): Validated[Violations, A] = codec match
    case Constant.Modify(self, f, _) => apply(codec = self, value).map(f)
    case Constant.Root(codec, eq, _) =>
      JsonKeyParser(codec = codec.self.value, value).andThen: a =>
        Validated
          .cond(
            test = eq.eqv(a, codec.value),
            (),
            Violation.equal(reference = JsonKeyPrinter(codec = codec.self.value, codec.value), value)
          )
          .leftMap(Violations.rootNec)

  def apply[A](codec: Enumeration[Json.Key.Primitive, A], value: String): Validated[Violations, A] = codec match
    case Enumeration.Modify(self, f, _) => apply(codec = self, value).map(f)
    case Enumeration.Root(reference, mapping, _) =>
      apply(codec = reference.value, value).andThen: a =>
        mapping
          .unapply(a)
          .toValid:
            val values = codec.values.map(mapping.apply).map(JsonKeyPrinter(reference.value, _))
            Violation.oneOf(values = values.toList, actual = value)
          .leftMap(Violations.rootNec)

  def apply[A](codec: Primitive.String[A], value: String): Validated[Violations, A] = codec match
    case Primitive.String.Parser(name, decode, _, minimum, maximum, matches, _) =>
      apply(minimum, maximum, matches)(value).andThen: value =>
        decode(value).toValidated.leftMap(Violation.tpe(name, actual = value, _)).leftMap(Violations.rootNec)
    case Primitive.String.Text(minimum, maximum, matches, _) =>
      apply(minimum, maximum, matches)(value)
    case Primitive.String.Modify(self, f, _) => apply(codec = self, value).map(f)

  def apply(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern])(
      value: String
  ): Validated[Violations, String] =
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
    ) *> matches
      .traverse(pattern =>
        Validated.cond(
          test = pattern.matcher(value).matches(),
          (),
          Violations.rootNec(
            Violation(constraint = Constraint.Primitive.Matches(pattern), actual = length, hint = none)
          )
        )
      )
      .as(value)

  def apply[A](codec: Union.Untagged[Json.Key, A], value: String): Validated[Violations, A] = codec match
    case Union.Untagged.OrElse(left, right, _) => apply(left, right, value)
    case Union.Untagged.Branch(name, codec, _) => apply(codec = codec.value, value).leftMap(name /: _)
    case Union.Untagged.Modify(self, f, _)     => apply(codec = self, value).map(f)

  def apply[A, B](
      left: Union.Untagged[Json.Key, A],
      right: Union.Untagged[Json.Key, B],
      value: String
  ): Validated[Violations, Either[A, B]] = apply(codec = left, value)
    .map(_.asLeft)
    .findValid(apply(codec = right, value).map(_.asRight))
