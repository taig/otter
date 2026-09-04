package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Parameter
import io.taig.validation.Comparison
import io.taig.validation.Violation

/** Reads a parameter out of the pieces of text its position holds, which is the inverse of what [[ParameterEncoder]]
  * writes. `delimiter` means the same thing here, the other way round.
  *
  * A parameter that is not a repetition insists on exactly one piece of text. Given more than one it reports an arity
  * violation rather than picking one, because which one it would pick -- the first, the last -- is a decision no caller
  * ever made, and quietly making it is how a parameter smuggled in twice comes to matter.
  */
final class ParameterDecoder(delimiter: Option[String]) extends Decoder[Parameter.Node, Chain[String]]:
  override def decode[R](parameter: Parameter.Node[Nothing, R], values: Chain[String]): Validated[Violations, R] =
    parameter match
      case Parameter.Collection.Schema(node) =>
        val elements = delimiter.fold(values): delimiter =>
          values.flatMap(value => Chain.fromSeq(value.split(delimiter, -1).toIndexedSeq)).map(_.trim)

        CollectionDecoder(ParameterValueDecoder).decode(node.self, elements.toList)
      case parameter @ Parameter.Coerce.Schema(_)            => single(parameter, values)
      case parameter @ Parameter.Constant.Schema(_)          => single(parameter, values)
      case parameter @ Parameter.Enumeration.Schema(_)       => single(parameter, values)
      case parameter @ Parameter.Primitive.Boolean.Schema(_) => single(parameter, values)
      case parameter @ Parameter.Primitive.Number.Schema(_)  => single(parameter, values)
      case parameter @ Parameter.Primitive.Text.Schema(_)    => single(parameter, values)

  private def single[R](
      parameter: Parameter.Value.Node[Nothing, R],
      values: Chain[String]
  ): Validated[Violations, R] = values.uncons match
    case Some((value, remainders)) if remainders.isEmpty => ParameterValueDecoder.decode(parameter, value)
    case Some(_)                                         =>
      Violations(
        Violation(
          constraint = Constraint.Collection.Maximum(Comparison(1, exclusive = false)),
          actual = values.length.toInt,
          hint = none
        )
      ).invalid
    case None =>
      Violations(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none)).invalid

object ParameterDecoder:
  /** A repetition read from the name given again for each element, which is what a query string does. */
  val Repeated: ParameterDecoder = new ParameterDecoder(delimiter = None)

  /** A repetition read from one value with the elements joined, which is what a header does. */
  val Delimited: ParameterDecoder = new ParameterDecoder(delimiter = Some(","))
