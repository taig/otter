package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter as Self
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.Fields
import io.taig.otter.codec.RecordDecoder
import io.taig.otter.http.Parameter
import io.taig.otter.http.Queries
import io.taig.otter.http.Query

import scala.collection.immutable.ListMap

/** Reads a query string out of its name and value pairs.
  *
  * A name given more than once is one parameter holding several values rather than several parameters, so the pairs are
  * grouped before the record reads them and a repetition reaches [[ParameterDecoder]] whole. A name given without a
  * value stands for empty text, which is what a strict boolean reads as the flag `?verbose` is.
  *
  * A repetition is also seeded, with no values, whether the request gave it or not. A query string cannot tell a
  * parameter given zero times apart from one never mentioned -- there is no `?tags=[]` -- so it is the schema that has
  * to say which of the two a missing name is, and for a repetition the answer is always "none of them". Without the
  * seed, [[QueriesEncoder]] writing nothing for an empty list and the record then demanding the name would make an
  * endpoint holding a required repetition one that cannot round trip its own empty case.
  *
  * Pairs the schema does not name are left where they are. Unlike a path, whose arity is the router's match, a query
  * string is open: a client appending a cache buster or an analytics tag has not made a different request.
  */
object QueriesDecoder extends Decoder[Queries.Node, Chain[(String, Option[String])]]:
  private val record = RecordDecoder(QueryDecoder)

  override def decode[R](
      queries: Queries.Node[Nothing, R],
      values: Chain[(String, Option[String])]
  ): Validated[Violations, R] =
    val schema = queries.self.self

    record.decode(schema, QueriesDecoder.group(QueriesDecoder.repetitions(schema), values))

  /** The names the schema declares whose parameter is a repetition. */
  private def repetitions(schema: Self.Record[Query.Node, Nothing, Any]): Chain[String] =
    schema.fields.map(_.value.self.self).filter(QueriesDecoder.repeats).map(_.name)

  private def repeats(field: Self.Field[Parameter.Node, Nothing, Any]): Boolean = field.schema.value match
    case Parameter.Collection.Schema(_) => true
    case Parameter.Coerce.Schema(_) | Parameter.Constant.Schema(_) | Parameter.Enumeration.Schema(_) |
        Parameter.Primitive.Boolean.Schema(_) | Parameter.Primitive.Number.Schema(_) |
        Parameter.Primitive.Text.Schema(_) =>
      false

  private def group(
      repetitions: Chain[String],
      values: Chain[(String, Option[String])]
  ): Fields[Chain[String]] =
    val seeded = repetitions.foldLeft(ListMap.empty[String, Chain[String]])(_.updated(_, Chain.empty))

    Fields.from:
      values
        .foldLeft(seeded): (grouped, pair) =>
          val (name, value) = pair

          grouped.updated(name, grouped.getOrElse(name, Chain.empty) :+ value.getOrElse(""))
