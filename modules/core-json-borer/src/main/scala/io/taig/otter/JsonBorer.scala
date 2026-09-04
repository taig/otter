package io.taig.otter

import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.all.*
import io.bullet.borer.Decoder as BorerDecoder
import io.bullet.borer.Dom
import io.bullet.borer.Encoder as BorerEncoder
import io.taig.data.Data
import io.taig.otter.codec.JsonBorerDecoder
import io.taig.otter.codec.JsonBorerEncoder

/** The borer codecs a JSON schema translates into.
  *
  * `JsonBorerDecoder` and `JsonBorerEncoder` interpret a schema against borer, which is one step short of what a borer
  * shaped consumer asks for: an `io.bullet.borer.Decoder[A]` and an `io.bullet.borer.Encoder[A]`. This is that step,
  * and with it the translation of [[Violations]] into what borer's error type has room for, which no consumer should be
  * reimplementing.
  *
  * Where the circe bridge and this one part company is the failure. circe's `DecodingFailure` is a value holding a
  * `List[CursorOp]`, so a violation's path survives translation and `decodeAccumulating` can report a whole list of
  * them. borer's `Borer.Error` is a message and an input position, `Reader.validationFailure` throws one, and
  * `valueEither` catches nothing else -- so a path has to be rendered into the message, and the accumulating side
  * cannot be a second method on the decoder. It is [[validated]] instead: a decoder of the `Validated` itself, which
  * never fails, and which a caller who wants every violation asks for in place of [[decoder]].
  */
object JsonBorer:
  /** A decoder reporting the first failure [[failures]] found, which is what every single failure caller reads.
    *
    * The whole document is read into a `Dom.Element` before the schema sees any of it, and that is not a shortcut
    * around borer's streaming reader. A schema driven read needs to go backwards: a record reads its members by name,
    * in schema order rather than document order, and a union tries its branches until one of them takes. borer's
    * `Reader` has one data item of lookahead and no way to rewind, and a failed read leaves it unusable, which would
    * also cost the accumulation that makes a `Validated` worth returning. Use [[validated]] for that accumulation.
    */
  def decoder[A](schema: Json.Reader[A]): BorerDecoder[A] = JsonBorer.decoder(schema, JsonBorer.message)

  /** The same decoder, reporting through a caller's own translation.
    *
    * An overload rather than a `using` parameter with [[message]] as the default given: `Violations => String` is far
    * too plain a type to leave to implicit search, where an unrelated function given could win it and the call site
    * would not say which error shape it got.
    */
  def decoder[A](schema: Json.Reader[A], failure: Violations => String): BorerDecoder[A] = BorerDecoder: reader =>
    JsonBorer.validated(schema).read(reader) match
      case Validated.Valid(value)   => value
      case Validated.Invalid(found) => reader.validationFailure(failure(found))

  /** Every failure, because accumulating them is the whole reason the interpreter returns a `Validated`, and borer's
    * error type has nowhere to hang more than one of them. A decoder that never fails, so `valueEither` is never the
    * thing that reports a violation.
    */
  def validated[A](schema: Json.Reader[A]): BorerDecoder[Validated[Violations, A]] = BorerDecoder: reader =>
    JsonBorerDecoder.decode(schema, reader.read[Dom.Element]())

  def encoder[A](schema: Json.Writer[A]): BorerEncoder[A] = BorerEncoder: (writer, value) =>
    JsonBorerEncoder.encode(schema, value).write(writer)

  /** Every [[Violation]] in the tree as a message of its own, each carrying the path it was found at, in a documented
    * order.
    *
    * Nothing is dropped, and the order is the same contract `JsonCirce.failures` states, because [[message]] is defined
    * as the first of these and a caller that can report only one may well pick the first itself:
    *
    *   - depth first,
    *   - a node's own violations before its children's,
    *   - children in [[Step]] order, which is what [[Violations]] already sorts by,
    *   - and the violations at one node in the order the `Validation` reported them, which is declaration order because
    *     `Validation.and` concatenates rather than short circuits.
    */
  def failures(violations: Violations): NonEmptyList[String] =
    JsonBorer.found(violations).map((path, message) => JsonBorer.describe(path, message))

  /** The single message a caller that can only report one gets: the first [[failures]] produces, carrying every failure
    * at that same path joined by `", "`.
    *
    * Which node wins therefore follows from the [[failures]] order: the first node holding violations of its own,
    * reached by descending into the smallest [[Step]] at every level.
    */
  def message(violations: Violations): String =
    val found = JsonBorer.found(violations)
    val path = found.head._1

    JsonBorer.describe(path, found.filter(_._1 === path).map(_._2).mkString_(", "))

  /** The path to a node, as it reads in a document.
    *
    * `path` is in reading order, outermost step first, which is the order [[Violations]] is indexed in, and [[Step]]
    * already renders as `.field` and `[0]`. Unlike `JsonCirce.history` there is nothing to reverse here: that reversal
    * exists only because circe keeps its cursor history deepest first.
    */
  def render(path: List[Step]): String = path.mkString

  /** The name of the kind of value a document holds, which is what a type mismatch is reported against.
    *
    * The six names JSON has, plus the two a `Dom` reached from CBOR rather than from JSON can hold. A schema names the
    * type it wanted; this names the type that arrived.
    */
  def typeOf(element: Dom.Element): String = element match
    case Dom.NullElem             => "null"
    case Dom.UndefinedElem        => "undefined"
    case Dom.BooleanElem(_)       => "boolean"
    case JsonBorerNumber(_)       => "number"
    case _: Dom.AbstractTextElem  => "string"
    case _: Dom.AbstractBytesElem => "bytes"
    case _: Dom.ArrayElem         => "array"
    case _: Dom.MapElem           => "object"
    case _                        => "unknown"

  /** A document as the format agnostic value a [[Violation]] reports.
    *
    * The number ladder is `data-circe`'s, rung for rung, and that agreement is the whole point of it: a violation this
    * module reports and a violation the circe module reports for the same document have to be the same value, and both
    * of them go through [[Data]], which compares with universal equality.
    */
  def toData(element: Dom.Element): Data = element match
    case Dom.NullElem            => Data.Null
    case Dom.BooleanElem(value)  => value
    case JsonBorerNumber(number) =>
      number.toInt
        .orElse(number.toLong)
        .orElse(number.toFloat.some.filter(value => value != Float.NegativeInfinity && value != Float.PositiveInfinity))
        .orElse(
          number.toDouble.some.filter(value => value != Double.NegativeInfinity && value != Double.PositiveInfinity)
        )
        .orElse(number.toBigInteger)
        .orElse(number.toBigDecimal)
        // Last resort, and deliberate: no exact representation exists in any of the types above, and `toDouble`'s own
        // contract is "nearest Double", of which an infinity is the correct one for a magnitude this large.
        .getOrElse(number.toDouble)
    case element: Dom.AbstractTextElem => element.compact
    case element: Dom.ArrayElem        => Data.Array(element.elems.map(JsonBorer.toData).toList)
    case element: Dom.MapElem          =>
      Data.Object(element.members.map((key, value) => JsonBorer.text(key) -> JsonBorer.toData(value)).toList)
    // A CBOR element with no JSON counterpart, reported as what it is rather than as a value it is not.
    case element => JsonBorer.typeOf(element)

  private def found(violations: Violations): NonEmptyList[(List[Step], String)] =
    def loop(violations: Violations, path: List[Step]): NonEmptyList[(List[Step], String)] = violations match
      case Violations.Root(values, found) =>
        found.toNonEmptyList.map(violation => path -> violation.hint.getOrElse(violation.constraint.show)) ++
          values.toList.flatMap((step, nested) => loop(nested, path :+ step).toList)
      case Violations.Namespace(values) => values.toNel.flatMap((step, nested) => loop(nested, path :+ step))

    loop(violations, path = Nil)

  private def describe(path: List[Step], message: String): String =
    if path.isEmpty then message else show"${JsonBorer.render(path)}: $message"

  /** A map key as the text it has to be to name a path. A `Dom` built by hand can key a map with anything; a document
    * borer parsed cannot, and neither can one this module writes.
    */
  private def text(element: Dom.Element): String = element match
    case element: Dom.AbstractTextElem => element.compact
    case element                       => element.toString
