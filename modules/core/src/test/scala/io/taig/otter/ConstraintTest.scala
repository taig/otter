package io.taig.otter

import cats.Eq
import cats.parse.Parser
import cats.syntax.all.*
import munit.Compare
import munit.FunSuite
import munit.Location

import java.util.regex.Pattern

final class ConstraintTest extends FunSuite:
  given Eq[Parser.Error] = Eq.fromUniversalEquals

  def catsEq[A: Eq] = new Compare[A, A]:
    override def isEqual(obtained: A, expected: A): Boolean = obtained === expected

  def assertEquals[A: Eq](obtained: A, expected: A)(using location: Location): Unit =
    super.assertEquals(obtained, expected)(using location, catsEq)

  test("parse"):
    assertEquals(
      obtained = Constraint.parse("type \"foobar\""),
      expected = Constraint.Type("foobar").asRight
    )

    assertEquals(
      obtained = Constraint.parse("oneOf [\"foobar\",42,true]"),
      expected = Constraint.OneOf(List(Data.String("foobar"), Data.Number(42), Data.Boolean(true))).asRight
    )

    assertEquals(
      obtained = Constraint.parse("maxItems 3"),
      expected = Constraint.Collection.MaxItems(3).asRight
    )

    assertEquals(
      obtained = Constraint.parse("minItems 3"),
      expected = Constraint.Collection.MinItems(3).asRight
    )

    assertEquals(
      obtained = Constraint.parse("uniqueItems"),
      expected = Constraint.Collection.UniqueItems.asRight
    )

    assertEquals(
      obtained = Constraint.parse("maxProperties 3"),
      expected = Constraint.Object.MaxProperties(3).asRight
    )

    assertEquals(
      obtained = Constraint.parse("minProperties 3"),
      expected = Constraint.Object.MinProperties(3).asRight
    )

    assertEquals(
      obtained = Constraint.parse("matches \"foobar\""),
      expected = Constraint.Primitive.Matches(Pattern.compile("foobar")).asRight
    )

    assertEquals(
      obtained = Constraint.parse("lteq 3"),
      expected = Constraint.Primitive.Maximum(Comparison(Data.Number(3), exclusive = false)).asRight
    )

    assertEquals(
      obtained = Constraint.parse("lt 3"),
      expected = Constraint.Primitive.Maximum(Comparison(Data.Number(3), exclusive = true)).asRight
    )

    assertEquals(
      obtained = Constraint.parse("gteq 3"),
      expected = Constraint.Primitive.Minimum(Comparison(Data.Number(3), exclusive = false)).asRight
    )

    assertEquals(
      obtained = Constraint.parse("gt 3"),
      expected = Constraint.Primitive.Minimum(Comparison(Data.Number(3), exclusive = true)).asRight
    )

    assertEquals(
      obtained = Constraint.parse("maxLength 3"),
      expected = Constraint.Primitive.MaxLength(3).asRight
    )

    assertEquals(
      obtained = Constraint.parse("minLength 3"),
      expected = Constraint.Primitive.MinLength(3).asRight
    )

    assertEquals(
      obtained = Constraint.parse("multiple 3.5"),
      expected = Constraint.Primitive.Multiple(Data.Number(3.5f)).asRight
    )
