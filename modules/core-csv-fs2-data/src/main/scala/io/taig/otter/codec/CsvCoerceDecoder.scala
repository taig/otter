package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Coerce
import io.taig.otter.Csv
import io.taig.otter.Violations

/** Normalises a laxer cell before handing it to the primitive decoder.
  *
  * JSON coerces between types, which a cell has none of; what CSV has instead is surrounding whitespace and the several
  * spellings a spreadsheet writes a boolean as. Trimming has to be asked for rather than assumed, because a quoted cell
  * can mean its spaces.
  */
object CsvCoerceDecoder extends Decoder[[w, r] =>> Coerce[Csv.Primitive.Node, w, r], String]:
  override def decode[R](schema: Coerce[Csv.Primitive.Node, Nothing, R], value: String): Validated[Violations, R] =
    schema match
      case Coerce.Modify(self, f, _) => decode(self, value).map(f)
      case Coerce.Root(reference)    =>
        val primitive = reference.value
        CsvPrimitiveDecoder.decode(primitive, coerce(primitive, value))

  private def coerce[R](schema: Csv.Primitive.Node[Nothing, R], value: String): String = schema match
    case Csv.Primitive.Boolean.Schema(_) => booleans.getOrElse(value.trim.toLowerCase, value.trim)
    case Csv.Primitive.Number.Schema(_)  => value.trim.stripPrefix("+")
    case Csv.Primitive.Text.Schema(_)    => value.trim

  /** What a spreadsheet writes a boolean as. `true` and `false` in any casing are already accepted by the strict
    * decoder, so only the alternate spellings are listed here.
    */
  private val booleans: Map[String, String] = Map(
    "y" -> "true",
    "yes" -> "true",
    "on" -> "true",
    "1" -> "true",
    "n" -> "false",
    "no" -> "false",
    "off" -> "false",
    "0" -> "false"
  )
