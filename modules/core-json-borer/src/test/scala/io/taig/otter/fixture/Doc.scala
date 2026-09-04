package io.taig.otter.fixture

import io.bullet.borer.Dom
import io.bullet.borer.Json as BorerJson
import io.circe.Json as CirceJson
import io.circe.JsonNumber

import java.nio.charset.StandardCharsets.UTF_8

/** A JSON document, as the one representation neither interpreter owns.
  *
  * An agreement test needs one document in two models, and deriving one from the other would need a
  * `Dom.Element => io.circe.Json` -- which *is* the code under test, so a bug in it would cancel out and the test would
  * pass while both interpreters were wrong. A hand built pair of trees is worse: it decides the number question in the
  * fixture, when which element borer's parser chooses for a given lexeme is the whole crux.
  *
  * So a document is written down once as this, and the two representations are derived independently, through text.
  *
  * A number is a **lexeme** rather than a `Double` for exactly that reason. borer's parser keeps `412` as an `IntElem`,
  * `412.0` as a `DoubleElem` and `1e400` as the raw string, while circe keeps all three as the text they were written
  * as -- and whether those three read as the same `Int` is the claim worth testing. A model holding a `Double` would
  * answer that question in the fixture instead of leaving it to the parsers.
  */
enum Doc:
  case Null
  case Bool(value: Boolean)
  case Num(lexeme: String)
  case Str(value: String)
  case Arr(values: List[Doc])
  case Obj(values: List[(String, Doc)])

object Doc:
  def render(doc: Doc): String = doc match
    case Doc.Null        => "null"
    case Doc.Bool(value) => String.valueOf(value)
    case Doc.Num(lexeme) => lexeme
    case Doc.Str(value)  => Doc.quote(value)
    case Doc.Arr(values) => values.map(Doc.render).mkString("[", ",", "]")
    case Doc.Obj(values) =>
      values.map((key, value) => Doc.quote(key) + ":" + Doc.render(value)).mkString("{", ",", "}")

  /** borer's model, by borer's own parser: the only thing this hands to borer is bytes. */
  def toBorer(doc: Doc): Dom.Element =
    BorerJson.decode(Doc.render(doc).getBytes(UTF_8)).to[Dom.Element].value

  /** circe's model, built the way circe's parser builds it.
    *
    * `JsonNumber.fromString` is what validates a lexeme and hands it to the same constructors circe's parser uses, so a
    * number keeps the text it was written as -- which is what makes `toBigDecimal` answer with the document's own
    * scale. A hand written conversion nonetheless, and therefore something that could be wrong in the direction that
    * makes an agreement pass, which is what `DocTest` pins against `circe-parser`.
    */
  def toCirce(doc: Doc): CirceJson = doc match
    case Doc.Null        => CirceJson.Null
    case Doc.Bool(value) => CirceJson.fromBoolean(value)
    case Doc.Num(lexeme) =>
      JsonNumber.fromString(lexeme).map(CirceJson.fromJsonNumber).getOrElse(sys.error(s"not a JSON number: $lexeme"))
    case Doc.Str(value)  => CirceJson.fromString(value)
    case Doc.Arr(values) => CirceJson.fromValues(values.map(Doc.toCirce))
    case Doc.Obj(values) => CirceJson.fromFields(values.map((key, value) => key -> Doc.toCirce(value)))

  private def quote(value: String): String =
    val builder = new StringBuilder(value.length + 2)
    builder.append('"')

    value.foreach:
      case '"'                          => builder.append("\\\"")
      case '\\'                         => builder.append("\\\\")
      case '\b'                         => builder.append("\\b")
      case '\f'                         => builder.append("\\f")
      case '\n'                         => builder.append("\\n")
      case '\r'                         => builder.append("\\r")
      case '\t'                         => builder.append("\\t")
      case character if character < ' ' => builder.append("\\u%04x".format(character.toInt))
      case character                    => builder.append(character)

    builder.append('"').toString
