package io.taig.otter.fixture

import io.circe.Json as CirceJson
import io.circe.JsonNumber

/** [[Doc]] in circe's model, built the way circe's parser builds it.
  *
  * `JsonNumber.fromString` is what validates a lexeme and hands it to the same constructors circe's parser uses, so a
  * number keeps the text it was written as -- which is what makes `toBigDecimal` answer with the document's own scale.
  * A hand written conversion nonetheless, and therefore something that could be wrong in the direction that makes an
  * agreement pass, which is what `DocTest` pins against `circe-parser`.
  */
object CirceDoc:
  def toCirce(doc: Doc): CirceJson = doc match
    case Doc.Null        => CirceJson.Null
    case Doc.Bool(value) => CirceJson.fromBoolean(value)
    case Doc.Num(lexeme) =>
      JsonNumber.fromString(lexeme).map(CirceJson.fromJsonNumber).getOrElse(sys.error(s"not a JSON number: $lexeme"))
    case Doc.Str(value)  => CirceJson.fromString(value)
    case Doc.Arr(values) => CirceJson.fromValues(values.map(CirceDoc.toCirce))
    case Doc.Obj(values) => CirceJson.fromFields(values.map((key, value) => key -> CirceDoc.toCirce(value)))
