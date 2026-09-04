package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter as Self
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.RecordDecoder
import io.taig.otter.http.Header
import io.taig.otter.http.Headers

import java.util.Locale
import scala.collection.immutable.ListMap

/** Reads a header set out of its name and value lines.
  *
  * A header name is case insensitive, and a record reads its fields by the name they were declared under, so the
  * incoming names are respelled to the schema's before the record sees them. Doing it here rather than in a field
  * decoder of its own is what keeps [[io.taig.otter.codec.FieldDecoder]] the same one every format uses: only this
  * position has names that two spellings can both be.
  *
  * Lines the schema does not name are left where they are. A header set is open in a way no other position is -- a
  * proxy adds `X-Forwarded-For`, a browser adds half a dozen -- so an unnamed header can never be an error.
  */
object HeadersDecoder extends Decoder[Headers.Node, Chain[(String, String)]]:
  private val record = RecordDecoder(HeaderDecoder)

  override def decode[R](headers: Headers.Node[Nothing, R], values: Chain[(String, String)]): Validated[Violations, R] =
    val schema = headers.self.self

    record.decode(schema, HeadersDecoder.respell(HeadersDecoder.names(schema), values))

  /** The names the schema declares, in the order it declares them. */
  private def names(schema: Self.Record[Header.Node, Nothing, Any]): Chain[String] =
    schema.fields.map(_.value.self.self.name)

  /** The incoming lines under the schema's spelling of each name, grouped so that a name sent more than once reaches
    * [[ParameterDecoder]] as the one parameter it is. A name the schema never mentions keeps the spelling it arrived
    * with, since there is nothing to prefer over it.
    */
  private def respell(names: Chain[String], values: Chain[(String, String)]): Chain[(String, Chain[String])] =
    val canonical = names.toList.map(name => HeadersDecoder.fold(name) -> name).toMap

    Chain.fromSeq:
      values
        .foldLeft(ListMap.empty[String, Chain[String]]): (grouped, line) =>
          val (name, value) = line
          val key = canonical.getOrElse(HeadersDecoder.fold(name), name)

          grouped.updated(key, grouped.getOrElse(key, Chain.empty) :+ value)
        .toSeq

  /** `Locale.ROOT` rather than the default locale, which would fold `I` to `ı` in Turkish and lose `Content-Type` to a
    * machine's regional settings.
    */
  private def fold(name: String): String = name.toLowerCase(Locale.ROOT)
