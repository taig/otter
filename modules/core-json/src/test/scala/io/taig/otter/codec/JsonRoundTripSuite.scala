package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Json
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** That what an interpreter writes, it reads back.
  *
  * The one claim a differential test structurally cannot make. Two interpreters compared against each other agree
  * wherever they are wrong the same way, and a matched pair of bugs -- a write and a read wrong in the same direction
  * -- is exactly that: it agrees on the way in and is still wrong on the way out. Only a round trip catches it.
  *
  * What the value passes through in the middle is [[JsonInterpreter.roundTrip]]'s business, not the contract's.
  */
abstract class JsonRoundTripSuite(interpreter: JsonInterpreter) extends ZIOSpecDefault:
  /** What this interpreter round trips that the contract cannot ask about. Empty is the whole of it. */
  protected def extra: List[Spec[TestEnvironment & Scope, Any]] = Nil

  private def roundTrips[A](schema: Json[A], value: A): TestResult =
    assertTrue(interpreter.roundTrip(schema, value) == Validated.valid(value))

  private val contract: Spec[TestEnvironment & Scope, Any] = suite("contract")(
    test("case class"):
      check(gen.book)(roundTrips(json.book, _))
    ,
    test("enum through a union"):
      check(gen.shape)(roundTrips(json.shape, _))
    ,
    test("enum through a union whose branches read the same type"):
      check(gen.verdict)(roundTrips(json.verdict, _))
    ,
    test("optional field, omitted"):
      check(gen.note)(roundTrips(json.omittedTag, _))
    ,
    test("optional field, nullable"):
      check(gen.note)(roundTrips(json.nullableTag, _))
    ,
    test("two layers of absence, kept apart by a strict field"):
      check(Gen.option(Gen.option(Gen.int)))(roundTrips(json.nestedTag, _))
    ,
    test("enumeration"):
      check(gen.genre)(roundTrips(json.genre, _))
    ,
    test("recursive schema"):
      check(gen.tree(depth = 3))(roundTrips(json.tree, _))
    ,
    test("enum through a union whose branches are named by a value"):
      check(gen.shape)(roundTrips(json.taggedShape, _))
    ,
    test("dictionary with a typed key"):
      check(gen.editions)(roundTrips(json.editions, _))
    ,
    test("dictionary with an integer key, which the document holds as text"):
      check(gen.printings)(roundTrips(json.printings, _))
    ,
    test("a 15 field record"):
      check(gen.census)(roundTrips(json.census, _))
  )

  final override def spec: Spec[TestEnvironment & Scope, Any] =
    suite(interpreter.name + "RoundTripTest")((contract :: extra)*)
