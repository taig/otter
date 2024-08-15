package io.taig.otter.http.csv

import io.taig.otter.http as Http
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

trait Codecs extends Http.Types, Http.Codecs:

  // TODO
  def csv[A](
      codec: Product.Of[Data.Primitive, A],
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
    { case (charset, Data.Null) =>
      ???
    }
  )

object Codecs extends Codecs
