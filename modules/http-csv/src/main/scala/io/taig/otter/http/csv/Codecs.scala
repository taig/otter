package io.taig.otter.http.csv

import io.taig.otter.http as Http
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import cats.data.Validated

trait Codecs extends Http.Types, Http.Codecs:
  // TODO
  def csv[A](
      codec: Record.Of[Data.Primitive, A] | Tuple.Of[Data.Primitive, A],
      fallback: => Charset = StandardCharsets.UTF_8
  ): Body[A] = body(
    mediaType = mediaType.text.csv,
    codec,
    (charset, bytes) =>
      val text = new String(bytes, charset.getOrElse(fallback))
      // Validated
      //   .fromEither(Csv.parse(text))
      //   .bimap(
      //     error => Violations.rootNec(Violation.tpe("csv", error.toString)),
      //     csv =>
      //       Data.Array.fromSeq(csv.rows.map(row => Data.Array.fromSeq(row.toList.map(cell => Data.String(cell.value)))))
      //   )

      ???
    ,
    (charset, data) => ???
  )

object Codecs extends Codecs
