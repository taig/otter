package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Comparison
import io.taig.validation.Direction
import zio.Scope
import zio.test.*

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern

/** What a schema's `Validation` says, said as the filters effect pipes a schema through.
  *
  * The module is a table, and a table is worth asserting entry by entry: a filter named wrongly, or a bound moved the
  * wrong way, produces source that still prints and still parses and is simply not the constraint the schema carried.
  * Only the two constraints reachable through a `Json` fixture had ever been exercised, and only from the renderer.
  */
object ConstraintTypescriptEffectTest extends ZIOSpecDefault:
  private def filter(constraint: Constraint): Option[String] =
    ConstraintTypescriptEffect.filter(constraint).map(_.render)

  private def minimum(reference: io.taig.data.Data.Number, exclusive: Boolean): Option[String] =
    filter(Constraint.Primitive.Number.Minimum(Comparison(reference, exclusive)))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ConstraintTypescriptEffectTest")(
    suite("text")(
      test("a length is a length"):
        assertTrue(
          filter(Constraint.Primitive.Text.Minimum(Comparison(1L, exclusive = false)))
            .contains("Schema.minLength(1)"),
          filter(Constraint.Primitive.Text.Maximum(Comparison(64L, exclusive = false)))
            .contains("Schema.maxLength(64)")
        )
      ,
      /** A length is an integer, so an exclusive bound is the inclusive one next to it -- and the two move in opposite
        * directions, which is the part that is easy to get backwards.
        */
      test("an exclusive bound on a length is the inclusive one next to it"):
        assertTrue(
          filter(Constraint.Primitive.Text.Minimum(Comparison(1L, exclusive = true))).contains("Schema.minLength(2)"),
          filter(Constraint.Primitive.Text.Maximum(Comparison(9L, exclusive = true))).contains("Schema.maxLength(8)")
        )
      ,
      test("a pattern both flavours read alike becomes one"):
        assertTrue(
          filter(Constraint.Primitive.Text.Matches(Pattern.compile("^[a-z]+$")))
            .contains("Schema.pattern(/^[a-z]+$/u)")
        )
      ,
      /** The one constraint whose counterpart is only sometimes there. Saying nothing is safe; saying something else is
        * not.
        */
      test("a pattern the two flavours disagree about says nothing"):
        assertTrue(filter(Constraint.Primitive.Text.Matches(Pattern.compile("(?i)^a$"))).isEmpty)
    ),
    suite("number")(
      test("a bound names whether it includes its reference"):
        assertTrue(
          minimum(1L, exclusive = true).contains("Schema.greaterThan(1)"),
          minimum(1L, exclusive = false).contains("Schema.greaterThanOrEqualTo(1)"),
          filter(Constraint.Primitive.Number.Maximum(Comparison(9L, exclusive = true)))
            .contains("Schema.lessThan(9)"),
          filter(Constraint.Primitive.Number.Maximum(Comparison(9L, exclusive = false)))
            .contains("Schema.lessThanOrEqualTo(9)")
        )
      ,
      /** Unlike a length, a numeric bound is not moved: a number between 1 and 2 exists, so `greaterThan(1)` is not
        * `greaterThanOrEqualTo(2)`.
        */
      test("an exclusive numeric bound keeps its reference"):
        assertTrue(minimum(1L, exclusive = true).contains("Schema.greaterThan(1)"))
      ,
      test("a multiple is a multiple"):
        assertTrue(
          filter(Constraint.Primitive.Number.Multiple(5L)).contains("Schema.multipleOf(5)")
        )
      ,
      /** Every branch of the reference's own type, because a bound is written as whatever number the schema carried and
        * each of the six reaches the decimal differently.
        */
      test("every kind of number a bound may carry is spelled as itself"):
        assertTrue(
          minimum(42, exclusive = false).contains("Schema.greaterThanOrEqualTo(42)"),
          minimum(42L, exclusive = false).contains("Schema.greaterThanOrEqualTo(42)"),
          minimum(new JBigInteger("42"), exclusive = false).contains("Schema.greaterThanOrEqualTo(42)"),
          minimum(new JBigDecimal("4.25"), exclusive = false).contains("Schema.greaterThanOrEqualTo(4.25)")
        )
      ,
      /** The bug this suite would have caught. `new BigDecimal(double)` is exact, so `0.1` became a bound of
        * `0.1000000000000000055511151231257827021181583404541015625` -- which excludes `0.1` itself, and so rejects a
        * value the schema it was read from accepts.
        */
      test("a binary float bound is spelled the way its own text spells it"):
        assertTrue(
          minimum(0.1d, exclusive = false).contains("Schema.greaterThanOrEqualTo(0.1)"),
          minimum(0.1f, exclusive = false).contains(s"Schema.greaterThanOrEqualTo(${0.1f.toString})"),
          minimum(2.5d, exclusive = false).contains("Schema.greaterThanOrEqualTo(2.5)")
        )
    ),
    suite("collection")(
      test("a size is a size, and an exclusive one is the inclusive one next to it"):
        assertTrue(
          filter(Constraint.Collection.Minimum(Comparison(1L, exclusive = false))).contains("Schema.minItems(1)"),
          filter(Constraint.Collection.Maximum(Comparison(9L, exclusive = false))).contains("Schema.maxItems(9)"),
          filter(Constraint.Collection.Minimum(Comparison(1L, exclusive = true))).contains("Schema.minItems(2)"),
          filter(Constraint.Collection.Maximum(Comparison(9L, exclusive = true))).contains("Schema.maxItems(8)")
        )
      ,
      /** effect has no filter for these, and approximating one would validate something other than what the schema
        * says. Dropping is the documented answer.
        */
      test("what effect cannot say is dropped rather than approximated"):
        assertTrue(
          filter(Constraint.Collection.Unique).isEmpty,
          filter(Constraint.Collection.Sorted(Direction.Ascending)).isEmpty,
          filter(Constraint.Collection.Sorted(Direction.Descending)).isEmpty
        )
    ),
    /** A record's size has no counterpart either: effect constrains the fields a struct declares, not how many of them
      * a value carries.
      */
    suite("object")(
      test("the size of a record says nothing"):
        assertTrue(
          filter(Constraint.Object.Minimum(Comparison(1L, exclusive = false))).isEmpty,
          filter(Constraint.Object.Maximum(Comparison(9L, exclusive = false))).isEmpty
        )
    ),
    /** A generic constraint is about presence or identity rather than shape, and is enforced by the schema the filter
      * would have been piped onto.
      */
    suite("generic")(
      test("a generic constraint says nothing"):
        assertTrue(
          filter(Constraint.Generic.Required).isEmpty,
          filter(Constraint.Generic.Type("uuid")).isEmpty
        )
    ),
    suite("filters")(
      test("a chain of constraints keeps the order it was given and drops what has no counterpart"):
        val constraints = Chain[Constraint](
          Constraint.Primitive.Text.Minimum(Comparison(1L, exclusive = false)),
          Constraint.Collection.Unique,
          Constraint.Primitive.Text.Maximum(Comparison(9L, exclusive = false))
        )

        assertTrue(
          ConstraintTypescriptEffect.filters(constraints).map(_.render) ==
            List("Schema.minLength(1)", "Schema.maxLength(9)")
        )
      ,
      test("no constraints is no filters"):
        assertTrue(ConstraintTypescriptEffect.filters(Chain.empty).isEmpty)
    )
  )
