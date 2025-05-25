package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.escape
import io.taig.otter.http.Query

final class QueryValueEncoder(explode: Boolean, style: Query.Style) extends Encoder[Query.Value, Option[Seq[String]]]:
  override def encode[A](schema: Query.Value[A], a: A): Option[Seq[String]] = schema match
    case schema: Query.Value.Atom[A] => Seq(QueryValueAtomPrinter.encode(schema, a)).some
    case schema: Query.Value.Array[A] =>
      val values = QueryValueArrayEncoder.encode(schema, a)

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
    case Query.Value.Nullable(self) =>
      NullableEncoder(encoder = this, empty = none).encode(schema = self.self, a)
