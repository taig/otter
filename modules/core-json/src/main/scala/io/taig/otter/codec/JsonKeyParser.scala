package io.taig.otter.codec

import io.taig.otter.Json

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

import java.util.regex.Pattern

object JsonKeyParser extends Decoder[Json.Key, String]:
  override def decode[A](schema: Json.Key[A], value: String): Validated[Violations, A] = schema match
    case Json.Key.Constant(self)    => decode(schema = self, value)
    case Json.Key.Enumeration(self) => decode(schema = self, value)
    case Json.Key.Primitive(self)   => decode(schema = self, value)
    case Json.Key.Union(self)       => decode(schema = self, value)

  def decode[A](schema: Constant[Json.Key, A], value: String): Validated[Violations, A] = schema match
    case Constant.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Constant.Root(schema, eq, _) =>
      decode(schema = schema.self.value, value).andThen: a =>
        Validated
          .cond(
            test = eq.eqv(a, schema.value),
            (),
            Violation.equal(reference = JsonKeyPrinter(schema = schema.self.value, schema.value), value)
          )
          .leftMap(Violations.rootNec)

  def decode[A](schema: Enumeration[Json.Key.Primitive, A], value: String): Validated[Violations, A] = schema match
    case Enumeration.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Enumeration.Root(reference, mapping, _) =>
      decode(schema = reference.value, value).andThen: a =>
        mapping
          .unapply(a)
          .toValid:
            val values = schema.values.map(mapping.apply).map(JsonKeyPrinter(reference.value, _))
            Violation.oneOf(values = values.toList, actual = value)
          .leftMap(Violations.rootNec)

  def decode[A](schema: Primitive.String[A], value: String): Validated[Violations, A] = schema match
    case Primitive.String.Parser(name, f, _, minimum, maximum, matches, _) =>
      decode(minimum, maximum, matches)(value).andThen: value =>
        f(value).toValidated.leftMap(Violation.tpe(name, actual = value, _)).leftMap(Violations.rootNec)
    case Primitive.String.Text(minimum, maximum, matches, _) =>
      decode(minimum, maximum, matches)(value)
    case Primitive.String.Modify(self, f, _) => decode(schema = self, value).map(f)

  def decode(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern])(
      value: String
  ): Validated[Violations, String] =
    val length = value.length
    minimum.traverse(minimum =>
      Validated.cond(
        test = length >= minimum,
        (),
        Violations.rootNec(
          Violation(constraint = Constraint.Primitive.String.Minimum(reference = minimum), actual = length, hint = none)
        )
      )
    ) *> maximum.traverse(maximum =>
      Validated.cond(
        test = length <= maximum,
        (),
        Violations.rootNec(
          Violation(constraint = Constraint.Primitive.String.Maximum(reference = maximum), actual = length, hint = none)
        )
      )
    ) *> matches
      .traverse(pattern =>
        Validated.cond(
          test = pattern.matcher(value).matches(),
          (),
          Violations.rootNec(
            Violation(constraint = Constraint.Primitive.String.Matches(pattern), actual = length, hint = none)
          )
        )
      )
      .as(value)

  def decode[A](schema: Union[Json.Key, A], value: String): Validated[Violations, A] = schema match
    case Union.OrElse(left, right, _) => decode(left, right, value)
    case Union.Root(schema, _)        => decode(schema = schema.value, value)
    case Union.Modify(self, f, _)     => decode(schema = self, value).map(f)

  def decode[A, B](
      left: Union[Json.Key, A],
      right: Union[Json.Key, B],
      value: String
  ): Validated[Violations, Either[A, B]] = decode(schema = left, value)
    .map(_.asLeft)
    .findValid(decode(schema = right, value).map(_.asRight))
