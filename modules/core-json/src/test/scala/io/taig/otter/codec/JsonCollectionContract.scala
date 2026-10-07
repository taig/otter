package io.taig.otter.codec

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.collections
import io.taig.otter.fixture.violations
import io.taig.validation.Comparison
import io.taig.validation.Violation
import io.taig.validation.std
import zio.Scope
import zio.test.*

import scala.collection.immutable.SortedSet

object JsonCollectionContract:
  def decoder(interpreter: JsonInterpreter): Spec[TestEnvironment & Scope, Any] =
    suite("collection shapes")(
      test("non-empty sequences preserve arrival order and duplicates"):
        assertTrue(
          interpreter.decode(collections.linked, "[3,1,3]") == NonEmptyList.of(3, 1, 3).valid,
          interpreter.decode(collections.indexed, "[3,1,3]") == NonEmptyVector.of(3, 1, 3).valid,
          interpreter.decode(collections.chained, "[3,1,3]") == NonEmptyChain.of(3, 1, 3).valid
        )
      ,
      test("empty input has the ordinary minimum-one violation"):
        val expected = interpreter.decode(collection.list(int, std.collection.minimum[List[Int]](1)), "[]").swap
        assertTrue(collections.nonEmpty.forall(schema => interpreter.decode(schema, "[]").swap == expected))
      ,
      test("sets accept unordered input and only the ordinary set accepts empty input"):
        assertTrue(
          interpreter.decode(collections.sorted, "[3,1,2]") == SortedSet(1, 2, 3).valid,
          interpreter.decode(collections.nonEmptySorted, "[3,1,2]") == NonEmptySet.of(1, 2, 3).valid,
          interpreter.decode(collections.sorted, "[]") == SortedSet.empty[Int].valid
        )
      ,
      test("sets reject duplicate occurrences before construction"):
        val expected = Violations(Violation(Constraint.Collection.Unique, Data.Array(List(2, 3)), None))
        assertTrue(
          interpreter.decode(collections.sorted, "[2,1,2,1]") == expected.invalid,
          interpreter.decode(collections.nonEmptySorted, "[2,1,2,1]") == expected.invalid
        )
      ,
      test("the read ordering defines duplicates, even for distinct values"):
        val expected = Violations(Violation(Constraint.Collection.Unique, Data.Array(List(1)), None))
        assertTrue(
          interpreter.decode(collections.byLength, """["a","b"]""") == expected.invalid,
          interpreter.decode(collections.nonEmptyByLength, """["a","b"]""") == expected.invalid
        )
      ,
      test("caller maximum validations apply to every new shape"):
        val expected = Constraint.Collection.Maximum(Comparison(2L, exclusive = false))
        assertTrue(
          collections.bounded.forall(schema => interpreter.decode(schema, "[1,2]").isValid),
          collections.bounded.forall(schema =>
            interpreter.decode(schema, "[1,2,3]").fold(violations.constraints, _ => Nil) == List(expected)
          )
        )
      ,
      test("stronger minima and impossible maxima remain effective"):
        val minimum = collection.nonEmptyList(int, std.collection.minimum[NonEmptyList[Int]](2))
        val impossible = collection.nonEmptyList(int, std.collection.maximum[NonEmptyList[Int]](0))
        assertTrue(
          interpreter.decode(minimum, "[]").isInvalid,
          interpreter.decode(minimum, "[1]").isInvalid,
          interpreter.decode(minimum, "[1,2]").isValid,
          interpreter.decode(impossible, "[]").isInvalid,
          interpreter.decode(impossible, "[1]").isInvalid
        )
      ,
      test("element failures accumulate at their wire paths before collection validation"):
        val schema = field("items", collections.nonEmptySorted).toRecord
        val result = interpreter.decode(schema, """{"items":["bad",1,"also bad"]}""")
        assertTrue(
          result.fold(violations.paths, _ => Nil) ==
            List(List(Step.Field("items"), Step.Index(0)), List(Step.Field("items"), Step.Index(2)))
        )
      ,
      test("asymmetric element schemas keep their read types"):
        assertTrue(interpreter.decode(collections.asymmetric, """["3","1"]""") == SortedSet(1L, 3L).valid)
    )

  def encoder(interpreter: JsonInterpreter): Spec[TestEnvironment & Scope, Any] =
    suite("collection shapes")(
      test("non-empty sequences write in arrival order"):
        assertTrue(
          interpreter.encode(collections.linked, NonEmptyList.of(3, 1, 3)) == "[3,1,3]",
          interpreter.encode(collections.indexed, NonEmptyVector.of(3, 1, 3)) == "[3,1,3]",
          interpreter.encode(collections.chained, NonEmptyChain.of(3, 1, 3)) == "[3,1,3]"
        )
      ,
      test("the schema ordering controls set writes"):
        val reverse = SortedSet(1, 2, 3)(using Ordering.Int.reverse)
        val nonEmptyReverse = NonEmptySet.fromSetUnsafe(reverse)
        assertTrue(
          interpreter.encode(collections.sorted, reverse) == "[1,2,3]",
          interpreter.encode(collections.nonEmptySorted, nonEmptyReverse) == "[1,2,3]",
          interpreter.encode(collections.reversed, SortedSet(1, 2, 3)) == "[3,2,1]",
          interpreter.encode(collections.nonEmptyReversed, NonEmptySet.of(1, 2, 3)) == "[3,2,1]"
        )
      ,
      test("sorting a supplied set never collapses its elements"):
        assertTrue(
          interpreter.encode(collections.byLength, SortedSet("a", "b", "cc")) == """["a","b","cc"]""",
          interpreter.encode(collections.nonEmptyByLength, NonEmptySet.of("a", "b", "cc")) == """["a","b","cc"]"""
        )
      ,
      test("asymmetric element schemas keep their write types"):
        assertTrue(interpreter.encode(collections.asymmetric, SortedSet(3, 1)) == """["1","3"]""")
      ,
      test("encoders do not apply read validation"):
        val schema = collection.nonEmptyList(int, std.collection.maximum[NonEmptyList[Int]](0))
        assertTrue(interpreter.encode(schema, NonEmptyList.one(1)) == "[1]")
    )

  def roundTrip(interpreter: JsonInterpreter): Spec[TestEnvironment & Scope, Any] =
    suite("collection shapes")(
      test("every shape round trips"):
        check(Gen.listOfBounded(1, 20)(Gen.int(-100, 100))): values =>
          val linked = NonEmptyList.fromListUnsafe(values)
          val indexed = NonEmptyVector.fromVectorUnsafe(values.toVector)
          val chained = NonEmptyChain.fromSeq(values).get
          val sorted = SortedSet.from(values)
          val nonEmptySorted = NonEmptySet.fromSetUnsafe(sorted)
          assertTrue(
            interpreter.roundTrip(collections.linked, linked) == linked.valid,
            interpreter.roundTrip(collections.indexed, indexed) == indexed.valid,
            interpreter.roundTrip(collections.chained, chained) == chained.valid,
            interpreter.roundTrip(collections.sorted, sorted) == sorted.valid,
            interpreter.roundTrip(collections.nonEmptySorted, nonEmptySorted) == nonEmptySorted.valid
          )
      ,
      test("lazy recursive references and conversions retain non-empty collections"):
        val leaf = collections.Tree(NonEmptyList.one(None))
        val tree = collections.Tree(NonEmptyList.of(Some(leaf), None))
        assertTrue(interpreter.roundTrip(collections.tree, tree) == tree.valid)
    )
