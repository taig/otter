package io.taig.otter

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json as CirceJson
import io.taig.otter.codec.JsonCirceDecoder
import io.taig.otter.codec.JsonCirceEncoder
import io.taig.validation.Violation

/** The circe codecs a JSON schema translates into.
  *
  * `JsonCirceDecoder` and `JsonCirceEncoder` interpret a schema against `io.circe.Json`, which is one step short of
  * what a circe shaped consumer asks for: an `io.circe.Decoder[A]` and an `io.circe.Encoder[A]`. This is that step, and
  * with it the translation of [[Violations]] into circe's own error type, which no consumer should be reimplementing.
  */
object JsonCirce:
  /** A decoder reporting the first failure [[failures]] found, which is what `jsonOf` and every other single failure
    * caller reads.
    */
  def decoder[A](schema: Json.Reader[A]): Decoder[A] = JsonCirce.decoder(schema, JsonCirce.failure)

  /** The same decoder, reporting through a caller's own translation.
    *
    * An overload rather than a `using` parameter with [[failure]] as the default given: `Violations => DecodingFailure`
    * is too plain a type to leave to implicit search, where an unrelated function given could win it and the call site
    * would not say which error shape it got.
    */
  def decoder[A](schema: Json.Reader[A], failure: Violations => DecodingFailure): Decoder[A] = new Decoder[A]:
    override def apply(cursor: HCursor): Decoder.Result[A] =
      JsonCirceDecoder.decode(schema, cursor.value).toEither.leftMap(failure)

    /** Every failure, because accumulating them is the whole reason the decoder returns a `Validated`. This is what
      * `accumulatingJsonOf` reads, and it is not filtered through the single failure translation.
      */
    override def decodeAccumulating(cursor: HCursor): Decoder.AccumulatingResult[A] =
      JsonCirceDecoder.decode(schema, cursor.value).leftMap(JsonCirce.failures)

  def encoder[A](schema: Json.Writer[A]): Encoder[A] = JsonCirceEncoder.encode(schema, _)

  /** Every [[Violation]] in the tree as a failure of its own, in a documented order.
    *
    * Nothing is dropped. A [[Violations.Root]] carries both its own violations and the nodes below it, and a
    * [[Violations.Namespace]] carries every step that failed, so a document with a bad dictionary key beside a bad
    * value reports both.
    *
    * The order is a contract, because [[failure]] is defined as the first of these and a caller that can report only
    * one may well pick the first itself:
    *
    *   - depth first,
    *   - a node's own violations before its children's,
    *   - children in [[Step]] order, which is what [[Violations]] already sorts by,
    *   - and the violations at one node in the order the `Validation` reported them, which is declaration order because
    *     `Validation.and` concatenates rather than short circuits.
    */
  def failures(violations: Violations): NonEmptyList[DecodingFailure] =
    def loop(violations: Violations, path: List[Step]): NonEmptyList[DecodingFailure] = violations match
      case Violations.Root(values, found) =>
        found.toNonEmptyList.map(JsonCirce.failureAt(_, path)) ++
          values.toList.flatMap((step, nested) => loop(nested, path :+ step).toList)
      case Violations.Namespace(values) => values.toNel.flatMap((step, nested) => loop(nested, path :+ step))

    loop(violations, path = Nil)

  /** The single failure a caller that can only report one gets: the first [[failures]] produces, carrying every failure
    * at that same history joined by `", "`.
    *
    * Which node wins therefore follows from the [[failures]] order: the first node holding violations of its own,
    * reached by descending into the smallest [[Step]] at every level.
    */
  def failure(violations: Violations): DecodingFailure =
    val failures = JsonCirce.failures(violations)
    val history = failures.head.history

    DecodingFailure(
      message = failures.filter(_.history === history).map(_.message).mkString_(", "),
      ops = history
    )

  /** The path to a node as circe cursor history.
    *
    * `path` is in reading order, outermost step first, which is the order [[Violations]] is indexed in and the order
    * the path reads in a document. Circe keeps its history deepest first, so what comes back is reversed. That reversal
    * is the whole reason this exists rather than being inlined: handing `DecodingFailure` a path the right way round
    * turns `.monday[0]` into `[0].monday` with no type error anywhere to catch it.
    */
  def history(path: List[Step]): List[CursorOp] = path.reverse.map:
    case Step.Field(name)  => CursorOp.DownField(name)
    case Step.Index(value) => CursorOp.DownN(value)

  /** The name of the kind of value a document holds, which is what a type mismatch is reported against.
    *
    * The six names JSON has for what a value is. A schema names the type it wanted; this names the type that arrived.
    */
  def typeOf(json: CirceJson): String = json.fold(
    jsonNull = "null",
    jsonBoolean = _ => "boolean",
    jsonNumber = _ => "number",
    jsonString = _ => "string",
    jsonArray = _ => "array",
    jsonObject = _ => "object"
  )

  private def failureAt(violation: Violation[Constraint], path: List[Step]): DecodingFailure = DecodingFailure(
    message = violation.hint.getOrElse(violation.constraint.show),
    ops = JsonCirce.history(path)
  )
