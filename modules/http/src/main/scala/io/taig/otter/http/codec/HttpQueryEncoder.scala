package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.escape
import io.taig.otter.http.Http
import io.taig.otter.http.Query

final class HttpQueryEncoder(explode: Boolean, style: Query.Style) extends Encoder[Http.Query, Option[Seq[String]]]:
  override def encode[A](schema: Http.Query[A], a: A): Option[Seq[String]] = schema match
    case schema: Http.Query.Value[A] => Seq(HttpQueryValuePrinter.encode(schema, a)).some
    case schema: Http.Query.Array[A] =>
      val values = HttpQueryArrayEncoder.encode(schema, a)

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
    case Http.Query.Nullable(self) =>
      NullableEncoder(encoder = this, empty = none).encode(schema = self, a)
