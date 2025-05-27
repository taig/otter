package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.escape
import io.taig.otter.http.Query

final class QuerySchemaEncoder(explode: Boolean, style: Query.Style) extends Encoder[Query.Schema, Option[Seq[String]]]:
  override def encode[A](schema: Query.Schema[A], a: A): Option[Seq[String]] = schema match
    case schema: Query.Schema.Atom[A] => Seq(QuerySchemaAtomPrinter.encode(schema, a)).some
    case schema: Query.Schema.Array[A] =>
      val values = QuerySchemaArrayEncoder.encode(schema, a)

      if explode
      then values.some
      else if values.isEmpty then Seq.empty.some
      else
        style
          .match
            case Query.Style.Form           => values.map(escape(_, ",")).mkString_(",")
            case Query.Style.SpaceDelimited => values.map(escape(_, " ")).mkString_(" ")
            case Query.Style.PipeDelimited  => values.map(escape(_, "|")).mkString_("|")
          .pure[Seq]
          .some
    case Query.Schema.Nullable(self) =>
      NullableEncoder(encoder = this, empty = none).encode(schema = self.self, a)
