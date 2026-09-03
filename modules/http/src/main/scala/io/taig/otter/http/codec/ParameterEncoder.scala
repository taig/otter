package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Parameter

/** Writes a parameter as the pieces of text its position holds.
  *
  * `delimiter` is how a repetition is spelled, which is the position's business and not the value's. A query string
  * gives the name again for every element and so has none; a header joins them into one line and so has one. These are
  * OpenAPI's own defaults for the two positions -- `style: form, explode: true` for a query, `style: simple` for a
  * header -- named here as the one place that knows the difference.
  */
final class ParameterEncoder(delimiter: Option[String]) extends Encoder[Parameter.Node, Chain[String]]:
  override def encode[W](parameter: Parameter.Node[W, Any], w: W): Chain[String] = parameter match
    case Parameter.Collection.Schema(node) =>
      val values = Chain.fromSeq(CollectionEncoder(ParameterValueEncoder).encode(node.self, w))
      delimiter.fold(values)(delimiter => Chain.one(values.toList.mkString(delimiter)))
    case parameter @ Parameter.Coerce.Schema(_)            => Chain.one(ParameterValueEncoder.encode(parameter, w))
    case parameter @ Parameter.Constant.Schema(_)          => Chain.one(ParameterValueEncoder.encode(parameter, w))
    case parameter @ Parameter.Enumeration.Schema(_)       => Chain.one(ParameterValueEncoder.encode(parameter, w))
    case parameter @ Parameter.Primitive.Boolean.Schema(_) => Chain.one(ParameterValueEncoder.encode(parameter, w))
    case parameter @ Parameter.Primitive.Number.Schema(_)  => Chain.one(ParameterValueEncoder.encode(parameter, w))
    case parameter @ Parameter.Primitive.Text.Schema(_)    => Chain.one(ParameterValueEncoder.encode(parameter, w))

object ParameterEncoder:
  /** A repetition written as the name given again for each element, which is what a query string does. */
  val Repeated: ParameterEncoder = new ParameterEncoder(delimiter = None)

  /** A repetition written as one value with the elements joined, which is what a header does. */
  val Delimited: ParameterEncoder = new ParameterEncoder(delimiter = Some(","))
