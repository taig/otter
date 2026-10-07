package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.http.component.HttpComponent.*
import io.taig.validation.Comparison
import io.taig.validation.Violation
import io.taig.validation.std
import zio.Scope
import zio.test.*

import scala.collection.immutable.SortedSet

object CollectionParameterTest extends ZIOSpecDefault:
  private val linked = collection.nonEmptyList(int)
  private val indexed = collection.nonEmptyVector(int)
  private val chained = collection.nonEmptyChain(int)
  private val sorted = collection.sortedSet(int)
  private val nonEmptySorted = collection.nonEmptySet(int)
  private val empty = Violations(
    Violation(Constraint.Collection.Minimum(Comparison(1L, exclusive = false)), actual = 0L, hint = None)
  )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("CollectionParameterTest")(
    test("all collection shapes decode repeated query values"):
      val values = Chain("3", "1", "2")
      assertTrue(
        ParameterDecoder.Repeated.decode(linked, values) == NonEmptyList.of(3, 1, 2).valid,
        ParameterDecoder.Repeated.decode(indexed, values) == NonEmptyVector.of(3, 1, 2).valid,
        ParameterDecoder.Repeated.decode(chained, values) == NonEmptyChain.of(3, 1, 2).valid,
        ParameterDecoder.Repeated.decode(sorted, values) == SortedSet(1, 2, 3).valid,
        ParameterDecoder.Repeated.decode(nonEmptySorted, values) == NonEmptySet.of(1, 2, 3).valid
      )
    ,
    test("required missing non-empty queries fail minimum one; optional missing queries remain absent"):
      val required = query("items", linked).toRecord
      val optional = query("items", linked).optional.toRecord
      assertTrue(
        QueriesDecoder.decode(required, Chain.empty) == ("items" /: empty).invalid,
        QueriesDecoder.decode(optional, Chain.empty) == None.valid
      )
    ,
    test("delimited empty headers reach the intrinsic minimum"):
      assertTrue(
        HeadersDecoder.decode(header("items", linked).toRecord, Chain("items" -> "")) ==
          ("items" /: empty).invalid,
        ParameterDecoder.Delimited.decode(indexed, Chain("")) == empty.invalid,
        ParameterDecoder.Delimited.decode(chained, Chain("")) == empty.invalid,
        ParameterDecoder.Delimited.decode(nonEmptySorted, Chain("")) == empty.invalid,
        ParameterDecoder.Delimited.decode(sorted, Chain("")) == SortedSet.empty[Int].valid
      )
    ,
    test("query and header sets reject duplicates with their wire positions"):
      val duplicate = Violations(Violation(Constraint.Collection.Unique, Data.Array(List(2)), None))
      assertTrue(
        QueriesDecoder.decode(
          query("items", sorted).toRecord,
          Chain("items" -> Some("1"), "items" -> Some("2"), "items" -> Some("1"))
        ) ==
          ("items" /: duplicate).invalid,
        HeadersDecoder.decode(header("items", nonEmptySorted).toRecord, Chain("items" -> "1,2,1")) ==
          ("items" /: duplicate).invalid
      )
    ,
    test("schema order determines both repeated query and delimited header output"):
      val values = SortedSet(1, 2, 3)(using Ordering.Int.reverse)
      val nonEmpty = NonEmptySet.fromSetUnsafe(values)
      val queries = query("items", sorted).toRecord
      val headers = header("items", nonEmptySorted).toRecord
      val queryWire = QueriesEncoder.encode(queries, values)
      val headerWire = HeadersEncoder.encode(headers, nonEmpty)
      assertTrue(
        queryWire == Chain("items" -> Some("1"), "items" -> Some("2"), "items" -> Some("3")),
        headerWire == Chain("items" -> "1,2,3"),
        QueriesDecoder.decode(queries, queryWire) == values.valid,
        HeadersDecoder.decode(headers, headerWire) == nonEmpty.valid
      )
    ,
    test("non-empty sequences round trip through delimited text"):
      val values = NonEmptyList.of("a,b", "", "a,b")
      val schema = collection.nonEmptyList(string)
      val wire = ParameterEncoder.Delimited.encode(schema, values)
      assertTrue(ParameterDecoder.Delimited.decode(schema, wire) == values.valid)
    ,
    test("supplied size validation reaches parameters"):
      val schema = collection.nonEmptyList(int, std.collection.maximum[NonEmptyList[Int]](1))
      assertTrue(ParameterDecoder.Repeated.decode(schema, Chain("1", "2")).isInvalid)
  )
