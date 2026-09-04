package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Absence
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.http.Header
import io.taig.otter.http.Http

/** Writes a header as the lines it contributes under its name.
  *
  * [[ParameterEncoder.Delimited]] rather than the repeated form a query string uses, so a header holding several values
  * writes them joined into one line. The metadata decides what an absent header contributes, which is why this cannot
  * reach the underlying field through `contramapK`.
  */
object HeaderEncoder extends Encoder[Header.Node, Chain[(String, Chain[String])]]:
  private val omitting = FieldEncoder(ParameterEncoder.Delimited, absent = none)

  private val emptying = FieldEncoder(ParameterEncoder.Delimited, absent = Chain.one("").some)

  override def encode[W](header: Header.Node[W, Any], w: W): Chain[(String, Chain[String])] =
    val encoder = Http.absence(header.self.metadata) match
      case Absence.Omit  => omitting
      case Absence.Empty => emptying

    encoder.encode(header.self.self, w)
