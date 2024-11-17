package io.taig.otter.http.csv

import cats.syntax.all.*
import io.taig.otter.http as Http

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

trait Codecs extends Http.Types, Http.Codecs:
  def csv[A](
      codec: Record.Of[Data.Primitive, A],
      fallback: => Charset = StandardCharsets.UTF_8
  ): Body.Strict[List[A]] = body(
    mediaType = mediaType.text.csv,
    (_, _) =>
      // TODO parse CSV
      // val value = new String(bytes, charset.getOrElse(fallback))
      ???,
    (charset, as) =>
      val csv = codec match
        case codec: Record.Of[Data.Primitive, A] =>
          Csv(
            headers = codec.fields.toVector.map(_.name).toList.some,
            values = as.map(codec.printObject(_).toList.map { case (_, value) => ??? })
          )
        // case codec: Tuple.Required.Of[Data.Primitive, A] =>
        //   Csv(headers = none, values = as.map(codec.printArray(_).toList))

      csv.show.getBytes(charset.getOrElse(fallback))
  )

object Codecs extends Codecs
