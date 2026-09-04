package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Json
import io.taig.otter.Violations

/** One JSON interpreter, named at the three points every conformance suite holds it to.
  *
  * A document is **text** on both sides, which is the only form every interpreter has in common: `core-json-borer`
  * builds no document on the way out at all -- its encoder carries a deferred write -- so the wire is the only place
  * two of them can be compared. Text on the way in for the same reason read backwards: it is the document as the caller
  * wrote it, before any library has decided what an element of it is, which is what makes the same assertion mean the
  * same thing under every interpreter.
  *
  * Reading text means reading it through the interpreter's own parser, so the contract holds the bridge and the parser
  * together rather than the interpreter alone. That is the intent: what a caller hands a JSON library is bytes, and
  * what it gets back is what the contract is about. Text a parser rejects is a failure of the test, not a violation.
  *
  * [[roundTrip]] names no representation, because what a value passes through on the way back is the interpreter's own
  * business: circe goes through `io.circe.Json`, borer through bytes.
  */
trait JsonInterpreter:
  /** How this interpreter names itself in a suite, e.g. `JsonCirce`. */
  def name: String

  def decode[A](schema: Json.Reader[A], document: String): Validated[Violations, A]

  def encode[A](schema: Json.Writer[A], value: A): String

  def roundTrip[A](schema: Json[A], value: A): Validated[Violations, A]
